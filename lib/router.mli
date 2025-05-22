(** This module expects packets with a leading tag byte that we use to route to one of
   several output streams. *)
open! Core

open Hardcaml
open Hardcaml_axi

module Make
    (_ : sig
       val num_tags : int
     end)
    (Axi : Stream.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; up : 'a Axi.Source.t
      ; dns : 'a Axi.Dest.t list
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { up : 'a Axi.Dest.t
      ; dns : 'a Axi.Source.t list
      }
    [@@deriving hardcaml]
  end

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
