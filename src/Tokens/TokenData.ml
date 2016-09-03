

(* Parser in the Hadoop Streaming format. *)

type token = (string * EnhancedRegex.t) Stream.t

let read input 

