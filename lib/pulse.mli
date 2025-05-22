(** This is an IO module that pulses a signal once per packet it receives. *)
open! Core

open Hardcaml
open Hardcaml_axi

module Make (Axi : Stream.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; up : 'a Axi.Source.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { up : 'a Axi.Dest.t
      ; signal : 'a
      }
    [@@deriving hardcaml]
  end

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
