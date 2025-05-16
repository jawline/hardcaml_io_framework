(** This is an IO module that pulses a signal once per packet it receives. *)
open! Core

open Hardcaml
open Hardcaml_axi
open Signal

module Make (Axi : Stream.S) = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; in_ : 'a Axi.Source.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { in_ : 'a Axi.Dest.t
      ; signal : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create (_scope : Scope.t) ({ I.clock = _; clear = _; in_ } : _ I.t) =
    { O.in_ = { Axi.Dest.tready = vdd }; signal = in_.tvalid &: in_.tlast }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"pulse" ~instance create input
  ;;
end
