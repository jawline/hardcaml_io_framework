open! Core
open Hardcaml

module Make (_ : sig
    val serial_input_width : int
  end) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; in_valid : 'a
      ; in_data : 'a
      ; out_ready : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { out_valid : 'a
      ; out_data : 'a
      }
    [@@deriving hardcaml]
  end

  val hierarchical : capacity:int -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
