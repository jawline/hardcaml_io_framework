open! Core
open Hardcaml
open Signal
open! Always

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
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let%hw_var ctr =
      Variable.reg ~width:(num_bits_to_represent (buffer_beats - 1)) reg_spec
    in
    let data_parts =
      List.init
        ~f:(fun i -> reg ~enable:(ctr.value ==:. i) reg_spec in_data)
        (buffer_beats - 1)
    in
    compile
      [ when_
          in_valid
          [ ctr <-- mod_counter ~max:(buffer_beats - 1) ctr.value
          ; when_ (ctr.value ==:. buffer_beats - 1) [ ctr <--. 0 ]
          ]
      ];
    { O.out_valid = ctr.value ==:. buffer_beats - 1 &: in_valid
    ; out_data = concat_lsb (List.concat [ data_parts; [ in_data ] ])
    ; interim_data_buffered = ctr.value <>:. 0
    ; ready = ctr.value <:. buffer_beats - 2 |: out_ready
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"data_resize" create input
  ;;
end
