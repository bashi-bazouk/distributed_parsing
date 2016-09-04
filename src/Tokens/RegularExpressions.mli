val sprintf : ('a, unit, string) format -> 'a
module Enhanced :
  sig
    
    module CharacterSet = CharacterSets.ASCII
    type character_set = CharacterSets.ASCII.t
    
    type t =
        Nil
      | Start
      | End
      | Wildcard
      | CharacterSet of character_set
      | Group of t
      | Iterate of t * int * int option
      | Conjunction of t * t
      | Disjunction of t * t

    val read_escape_character : char Stream.t -> t

    val read_one : ?nesting:int -> char Stream.t -> t

    val read : ?nesting:int -> ?prev:t -> char Stream.t -> t

    val show : t -> string

    val print : t -> unit


    (* Analysis *)

    val destructure : t -> (t * t) list

    module Index : sig

      type boundary = Open | Close

      val cardinal: t -> t

      val cardinality: t -> int

      val basis: t -> (boundary * int) list

    end
  end
