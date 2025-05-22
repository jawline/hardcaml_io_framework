(** This module implements a register interface read/write protocol, allowing
    for interaction with a register bank. *)
open! Core

open Hardcaml
open Hardcaml_axi

module Make (Axi : Stream.S) (Internal_bus : Internal_bus.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; up : 'a Axi.Source.t
      ; dn : 'a Axi.Dest.t
      ; slave_to_master : 'a Internal_bus.Slave_to_master.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { up : 'a Axi.Dest.t
      ; dn : 'a Axi.Source.t
      ; master_to_slave : 'a Internal_bus.Master_to_slave.t
      }
    [@@deriving hardcaml]
  end

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
