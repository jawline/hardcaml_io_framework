(** This module implements a register interface read/write protocol, allowing
    for interaction with a register bank. *)
open! Core
open Hardcaml
open Hardcaml_axi

module Make (Axi : Stream.S) (Internal_bus : Internal_bus.S) = struct
  module Master_to_slave = Internal_bus.Master_to_slave
  module Slave_to_master = Internal_bus.Slave_to_master

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

  let create (_scope : Scope.t) (_i : _ I.t) =
          assert false
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"pulse" ~instance create input
  ;;
end
