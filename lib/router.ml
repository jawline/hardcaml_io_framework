open! Core
open Hardcaml
open Hardcaml_axi
open Signal
open! Always

(* This expects packets with a leading tag byte that we use to route to one of
   several output streams. *)

module Make
    (Config : sig
       val num_tags : int
     end)
    (Axi : Stream.S) =
struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; in_ : 'a Axi.Source.t
      ; outs : 'a Axi.Dest.t list [@length Config.num_tags]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { in_ : 'a Axi.Dest.t
      ; outs : 'a Axi.Source.t list [@length Config.num_tags]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module State = struct
    type t =
      | Waiting_for_start_of_packet
      | Routing
      | Discarding_bad_tag
    [@@deriving sexp, enumerate, compare]
  end

  let create (scope : Scope.t) ({ I.clock; clear; in_; outs } : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let state = State_machine.create (module State) reg_spec in
    ignore (state.current -- "current_state" : Signal.t);
    let%hw_var which_tag =
      Variable.reg ~width:(Signal.num_bits_to_represent (Config.num_tags - 1)) reg_spec
    in
    let selected_out_ready =
      let output_is_ready =
        (* TODO: We could register the which tag comparison bits instead. *)
        List.mapi
          ~f:(fun i out ->
            repeat ~count:(width out.tready) (which_tag.value ==:. i) &: out.tready)
          outs
        |> List.reduce_exn ~f:( |: )
      in
      state.is Routing
      &: output_is_ready
      |: state.is Waiting_for_start_of_packet
      |: state.is Discarding_bad_tag
    in
    compile
      [ state.switch
          [ ( State.Waiting_for_start_of_packet
            , [ which_tag <-- uresize ~width:(width which_tag.value) in_.tdata
              ; when_
                  in_.tvalid
                  [ (* If a received tag is out of the range of the
                       routable tags we discard the whole packet in the
                       router. *)
                    when_
                      ~:(in_.tlast)
                      [ if_
                          (in_.tdata <=:. Config.num_tags - 1)
                          [ state.set_next Routing ]
                          [ state.set_next Discarding_bad_tag ]
                      ]
                  ]
              ] )
          ; ( Routing
            , [ when_
                  (in_.tvalid &: in_.tlast)
                  [ state.set_next Waiting_for_start_of_packet ]
              ] )
          ; ( Discarding_bad_tag
            , [ when_
                  (in_.tvalid &: in_.tlast)
                  [ state.set_next Waiting_for_start_of_packet ]
              ] )
          ]
      ];
    { O.in_ = { tready = selected_out_ready }
    ; outs =
        List.init
          ~f:(fun index ->
            Axi.Source.Of_signal.mux2
              (which_tag.value ==:. index &: state.is Routing)
              in_
              (Axi.Source.Of_signal.of_int 0))
          Config.num_tags
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"packet_router" ~instance create input
  ;;
end
