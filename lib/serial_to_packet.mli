(** This expects packets from a serial port in the form [ header ; length MSB ;
   length LSB ; data ... ]. If a second packet is received before this packet
   is flushed out then it will be dropped so the serial sender is responsible
   for waiting for acknowledgement. *)
open! Core

open Hardcaml
open Hardcaml_axi

module Make
    (_ : sig
       val header : char
     end)
    (Axi : Stream.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; in_valid : 'a
      ; in_data : 'a
      ; dn : 'a Axi.Dest.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { dn : 'a Axi.Source.t
      ; up_ready : 'a
      }
    [@@deriving hardcaml]
  end

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
