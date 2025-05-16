(** This module implements a register interface read/write protocol, allowing
    for interaction with a register bank. *)
open! Core

open Hardcaml
open Signal
open Always
open Hardcaml_axi

module Tags = struct
  type t =
    | Read
    | Write
  [@@deriving enumerate]

  let to_rank = function
    | Read -> 0
    | Write -> 1
  ;;
end

module Make (Axi : Stream.S) (Internal_bus : Internal_bus.S) = struct
  module Master_to_slave = Internal_bus.Master_to_slave
  module Slave_to_master = Internal_bus.Slave_to_master

  module Resize_to_word = Data_resize.Make (struct
      let input_width = Axi.Source.port_widths.tdata
      let output_width = 32
    end)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; up : 'a Axi.Source.t
      ; dn : 'a Axi.Dest.t
      ; slave_to_master : 'a Slave_to_master.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { up : 'a Axi.Dest.t
      ; dn : 'a Axi.Source.t
      ; master_to_slave : 'a Master_to_slave.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module State = struct
    type t =
      | Waiting_for_tag
      | Read_wait_address
      | Read_wait_response
      | Read_write_response
      | Write_wait_address
      | Write_wait_data
      | Write_wait_ack
    [@@deriving sexp, enumerate, compare]
  end

  let create (scope : Scope.t) ({ clock; clear; up; dn; slave_to_master } : _ I.t) =
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let%hw.State_machine state = State_machine.create (module State) reg_spec in
    let word_collector =
      Resize_to_word.hierarchical
        scope
        { Resize_to_word.I.clock
        ; clear = clear |: state.is Waiting_for_tag
        ; in_valid = up.tvalid
        ; in_data = up.tdata
        ; out_ready = vdd
        }
    in
    let rd_data =
      reg ~enable:slave_to_master.read_ready reg_spec slave_to_master.read_data
    in
    let num_beats_to_write_word = width rd_data / Axi.Source.port_widths.tdata in
    let rd_resp_ctr =
      Variable.reg ~width:(address_bits_for num_beats_to_write_word) reg_spec
    in
    let latch ~enable t =
            mux2 enable t (reg ~enable reg_spec t) in
    compile
      [ state.switch
          [ ( Waiting_for_tag
            , [ rd_resp_ctr <--. 0
              ; when_
                  (up.tvalid &: ~:(up.tlast))
                  [ when_
                      (up.tdata ==:. Tags.to_rank Read)
                      [ state.set_next Read_wait_address ]
                  ; when_
                      (up.tdata ==:. Tags.to_rank Write)
                      [ state.set_next Write_wait_address ]
                  ]
              ] )
          ; ( Read_wait_address
            , [ (* This resets the state machine in cases where we see tlast
                   before the end of the read address. *)
                when_ (up.tvalid &: up.tlast) [ state.set_next Waiting_for_tag ]
              ; when_
                  word_collector.out_valid
                  [ 
                   state.set_next Read_wait_response
                  ]
              ] )
          ; ( Read_wait_response
            , [ when_ slave_to_master.read_ready [ state.set_next Read_write_response ] ]
            )
          ; ( Read_write_response
            , [ when_
                  dn.tready
                  [ incr rd_resp_ctr
                  ; when_
                      (rd_resp_ctr.value ==:. num_beats_to_write_word - 1)
                      [ state.set_next Waiting_for_tag ]
                  ]
              ] )
          ; ( Write_wait_address
            , [ when_ (up.tvalid &: up.tlast) [ state.set_next Waiting_for_tag ]
              ; when_
                  word_collector.out_valid
                  [ state.set_next Write_wait_data
                  ]
              ] )
          ; ( Write_wait_data
            , [ when_ (up.tvalid &: up.tlast) [ state.set_next Waiting_for_tag ]
              ; when_
                  word_collector.out_valid
                  [ if_ slave_to_master.write_ready [ state.set_next Waiting_for_tag ] [ state.set_next Write_wait_ack ] 
                  ]
            ] );
           ( Write_wait_ack, [
            when_ slave_to_master.write_ready [ state.set_next Waiting_for_tag ]
            ] )
          ]
      ];
    { O.up = { tready = vdd }
    ; dn =
        { Axi.Source.tvalid = state.is Read_write_response
        ; tdata =
            mux
              rd_resp_ctr.value
              (split_msb ~part_width:Axi.Source.port_widths.tdata rd_data)
        ; tkeep = ones Axi.Source.port_widths.tkeep
        ; tstrb = ones Axi.Source.port_widths.tstrb
        ; tlast = rd_resp_ctr.value ==:. num_beats_to_write_word - 1
        ; tuser = zero Axi.Source.port_widths.tuser
        }
    ; master_to_slave = 

            ( let first_beat_of_write = (state.is Write_wait_data &: word_collector.out_valid) in
            let first_beat_of_read = state.is Read_wait_address &: word_collector.out_valid in
{ write_valid = first_beat_of_write |: state.is Write_wait_ack

                      ; write_first = first_beat_of_write
                      ; read_valid = first_beat_of_read |: state.is Read_wait_response
                      ; read_first = first_beat_of_read
                      ; address = onehot_select [ 
                              { With_valid.valid = state.is Write_wait_data  |: state.is Write_wait_ack ; value = 
                              reg ~enable:(state.is Write_wait_address &: word_collector.out_valid) reg_spec (word_collector.out_data) 
                      } ; { With_valid.valid = state.is Read_wait_address  |: state.is Read_wait_response
                      
                      ; value = latch ~enable:first_beat_of_read word_collector.out_data }
      ]
                      ; write_data = latch ~enable:first_beat_of_write word_collector.out_data
                      ; write_byte_en = ones 4
                      })
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"register_interface" ~instance create input
  ;;
end
