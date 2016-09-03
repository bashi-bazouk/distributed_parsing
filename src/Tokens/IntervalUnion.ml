
(*
IntervalUnion represents the union of a set of integer-valued intervals.
The implementation is a variant of the red-black tree data-type.
*)



type t =
	| Nil
	| Step of left * right
	| Inversion of t
	| Union of t * t
and left = t * int * t
and right = t * int * t

(*
	Invariants
	1. Step((a, x, b), (c, y, d))  -->  a <= x <= b <= c <= y <= d
	2. Inversion(Inversion(Step(l,r))) == Step(l,r)
	3. Step = Black /\ Inversion(Step) = Red --> Red-Black Invariants:
		a) No red node has a red parent
		b) Every path from the root to an empty node has the same number of Black nodes.

		alt.
	
		a) No 


*)

let forget: 'a option -> unit option = function
	| None -> Some(()) 
	| Some _ -> None

let witness: 'a option -> bool = function
	| Some _ -> true
	| None -> false


let rec invert: t -> t = function
	| Inversion(Inversion(t)) ->
		(* Terms cancel.*)
		invert t

	| Nil -> Nil
	| Step((a,x,b),(c,y,d)) ->
		Step((a,x,b), invert (Invert(c), y, Invert(d)))

	| Inversion(t) ->


		if witness inverted then
			invert ~inverted:() t
		else
			t
	| Step(l, r) ->
		if inverted = None then
			Step(invert ?inverted:inverted l, 

let rec ground ?inverted: t -> t = function
	(* Base Cases *)
	| Leaf -> Leaf
	| Left of (a, i, b) ->
		Left (ground a, i, ground b)
	| Right of (c, i, d) ->
		Right (ground c, i, ground d)
	(* Inductions *)
	| 
	| Leaf -> Leaf

let rec inorder f = function
	| Leaf -> ()
	| Node(l, i, r) ->
		inorder f l;
		f i;
		inorder f r






type t =
	| Nil
	| Left of t * int * t
	| Right of t * int * t

let balance: t -> t = function
	| Right(Left(Left(a, x, b), y, c), z, d)
	| Right(Left(a, x, Left(b, y, c)), z, d)
	| Right(a, x, Left(Left(b, y, c), z, d))
	| Right(a, x, Left(b, y, Left(c, z, d))) ->





let insert ((l,r): int * int): t -> t = function
	| Nil -> Left(Nil, l, Right(Nil, r, Nil))
	| Left(l', pivot, r') ->


let validate_traversal = 
let preorder_traversal = function

