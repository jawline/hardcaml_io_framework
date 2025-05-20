open! Core
open Hardcaml
open Signal
open! Always
module Clocking = Types.Clocking

module Make (Config : sig
    val input_width : int
    val output_width : int
  end) =
struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; in_valid : 'a
      ; in_data : 'a [@bits Config.input_width]
      ; out_ready : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { out_valid : 'a
      ; out_data : 'a [@bits Config.output_width]
      ; interim_data_buffered : 'a
      ; ready : 'a
      }
    [@@deriving hardcaml]
  end

  let () =
    if Config.output_width % Config.input_width <> 0
    then raise_s [%message "BUG: Output must be a multiple of input width"]
  ;;

  let buffer_beats = Config.output_width / Config.input_width

  let create (scope : Scope.t) ({ I.clock; clear; in_valid; in_data; out_ready } : _ I.t) =
    let clocking = { Clocking.clock; clear } in
    let%hw_var ctr =
      Variable.reg
        ~width:(num_bits_to_represent (buffer_beats - 1))
        (Clocking.to_spec clocking)
    in
    let last_beat = ctr.value ==:. buffer_beats - 1 in
    let data_parts =
      List.init
        ~f:(fun i -> Clocking.reg ~enable:(ctr.value ==:. i) clocking in_data)
        (buffer_beats - 1)
    in
    compile
      [ if_
          last_beat
          [ when_ (in_valid &: out_ready) [ ctr <--. 0 ] ]
          [ when_ in_valid [ ctr <-- mod_counter ~max:(buffer_beats - 1) ctr.value ] ]
      ];
    { O.out_valid = last_beat &: in_valid
    ; out_data = concat_msb (data_parts @ [ in_data ])
    ; interim_data_buffered = ctr.value <>:. 0
    ; ready = ~:last_beat |: out_ready
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"data_resize" create input
  ;;
end
