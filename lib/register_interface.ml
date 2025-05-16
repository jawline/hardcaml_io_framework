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
      | Read_write_response
      | Write_wait_address
      | Write_wait_data
    [@@deriving sexp, enumerate, compare]
  end

  let create (scope : Scope.t) ({ clock; clear; up; dn = _; slave_to_master = _ } : _ I.t)
    =
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let state = State_machine.create (module State) reg_spec in
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
    let%hw_var write_address = Variable.reg ~width:32 reg_spec in
    compile
      [ state.switch
          [ ( Waiting_for_tag
            , [ when_
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
                  [ assert false; state.set_next Read_write_response ]
              ] )
          ; Read_write_response, []
          ; ( Write_wait_address
            , [ when_ (up.tvalid &: up.tlast) [ state.set_next Waiting_for_tag ]
              ; when_
                  word_collector.out_valid
                  [ write_address <-- word_collector.out_data
                  ; state.set_next Write_wait_data
                  ]
              ] )
          ; ( Write_wait_data
            , [ when_ (up.tvalid &: up.tlast) [ state.set_next Waiting_for_tag ]
              ; when_ word_collector.out_valid [ assert false ]
              ] )
          ]
      ];
    { O.up = assert false; dn = assert false; master_to_slave = assert false }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"pulse" ~instance create input
  ;;
end
