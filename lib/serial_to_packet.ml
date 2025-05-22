open! Core
open Hardcaml
open Signal
open! Always
open Hardcaml_axi
module Clocking = Types.Clocking

(* This expects packets from a serial port in the form [ header ; length MSB ;
   length LSB ; data ... ]. If a second packet is received before this packet
   is flushed out then it will be dropped so the serial sender is responsible
   for waiting for acknowledgement. *)

(* TODO: Consider adding a checksum. *)

module Make
    (Config : sig
       val header : char
     end)
    (Axi : Stream.S) =
struct
  let data_width = Axi.Source.port_widths.tdata

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; in_valid : 'a
      ; in_data : 'a [@bits data_width]
      ; dn : 'a Axi.Dest.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { dn : 'a Axi.Source.t
      ; up_ready : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module State = struct
    type t =
      | Waiting_for_start
      | Waiting_for_length
      | Streaming_in
    [@@deriving sexp, enumerate, compare]
  end

  let create
        (scope : Scope.t)
        ({ I.clock; clear; in_valid; in_data; dn = { tready = out_ready } } : _ I.t)
    =
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let clocking = { Clocking.clock; clear } in
    let%hw.State_machine state = State_machine.create (module State) reg_spec in
    let num_length_beats = 2 / (width in_data / 8) in
    let which_length_packet =
      Variable.reg ~width:(address_bits_for num_length_beats) reg_spec
    in
    let length_parts =
      List.init
        ~f:(fun i ->
          reg
            ~enable:(state.is Waiting_for_length &: (which_length_packet.value ==:. i))
            reg_spec
            in_data)
        num_length_beats
    in
    let total_length = concat_msb length_parts in
    let on_byte =
      Clocking.reg_fb
        ~width:(width total_length)
        ~enable:(state.is Streaming_in &: in_valid &: out_ready)
        ~clear_to:(of_unsigned_int ~width:(width total_length) 1)
        ~f:(fun t -> t +:. 1)
        (Clocking.add_clear clocking (state.is Waiting_for_start))
    in
    let last = on_byte ==: total_length in
    compile
      [ state.switch
          [ ( State.Waiting_for_start
            , [ which_length_packet <--. 0
              ; when_
                  (in_valid &: (in_data ==: of_char Config.header))
                  [ state.set_next Waiting_for_length ]
              ] )
          ; ( Waiting_for_length
            , let length_this_cycle =
                concat_msb
                  (List.take length_parts (List.length length_parts - 1) @ [ in_data ])
              in
              [ when_
                  in_valid
                  [ incr which_length_packet
                  ; when_
                      (which_length_packet.value ==:. num_length_beats - 1)
                      [ (* Zero length packets are disallowed *)
                        if_
                          (length_this_cycle ==:. 0)
                          [ state.set_next Waiting_for_start ]
                          [ state.set_next Streaming_in ]
                      ]
                  ]
              ] )
          ; ( Streaming_in
            , [ when_
                  (in_valid &: out_ready)
                  [ when_ last [ state.set_next Waiting_for_start ] ]
              ] )
          ]
      ];
    { O.dn =
        { Axi.Source.tvalid = in_valid &: state.is Streaming_in
        ; tdata = in_data
        ; tlast = last
        ; tkeep = ones Axi.Source.port_widths.tkeep
        ; tstrb = ones Axi.Source.port_widths.tstrb
        ; tuser = zero Axi.Source.port_widths.tuser
        }
    ; up_ready =
        state.is Waiting_for_start
        |: state.is Waiting_for_length
        |: (state.is Streaming_in &: out_ready)
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"serial_to_packet" create input
  ;;
end
