open Big_int
type integer = big_int


module type Union = sig
	(* Module to efficiently represent a union of integer-valued intervals. *)

	type t

	val union: (int * int) -> t -> t

	val union_big_int: (integer * integer) -> t -> t

end


module Union = struct
	(* Implemented as a modification of the red-black tree code, found at
	 * http://www.cs.cornell.edu/courses/cs3110/2014sp/lectures/11/red-black-trees.html *)

	 (*

		Pretend that a black node constitutes +1 extra space.
		Pretend that a red node constitutes -1 extra space.
		1. There are no two adjacent red nodes along any path.
		->
		2. Every path from the root to a leaf has the same number of black nodes.
		-> if invariant then forall path (fun path -> )
	 *)

	type red_black = Red | Black

	type 'coloring bst =
		| Node of 'coloring * integer * t * t
		| Leaf

	type t = red_black bst

	let insert_point (x: integer) (union: t): t =
		let (<) = lt_big_int
		and (>) = gt_big_int in

		let rec insert_point = function
			| Leaf -> Node (Red, x, Leaf, Leaf)
			| Node (color, y, a, b) as node ->
				if x < y then 
					Node (color, y, balance (insert_point a), b)
				else if x > y then
					Node (color, y, a, balance (insert_point b))
				else node

		and balance = function
			| Node (Black, z, (Node (Red, y, (Node (Red, x, a, b)), c)), d)
			| Node (Black, z, (Node (Red, x, a, (Node (Red, y, b, c)))), d)
			| Node (Black, x, a, (Node (Red, z, (Node (Red, y, b, c)), d)))
			| Node (Black, x, a, (Node (Red, y, b, (Node (Red, z, c, d))))) ->
				Node (Red, y, (Node (Black, x, a, b)), (Node (Black, z, c, d)))
			| default -> default

		and color_root_black = function
			| Node (Red, y, a, b) -> Node (Black, y, a, b)
			| default -> default in

		color_root_black (balance (insert_point union))


	(* Deletion *)

	let rec remove_maxima: t -> (t * integer * coloring) = function
		| Leaf ->
			raise Not_found
		| Node (color, maxima, l, Leaf)
			(l, maxima, color)
		| Node (color, x, l, r) ->
			let (r', maxima, color) = 
				remove_maxima r in
			(Node (color, x, l, r'), maxima, color)
	

	and delete_point (x: integer) (union: t): t =
		let (<) = lt_big_int
		and (>) = gt_big_int in

		let balance_tree_of_t = function
			| Leaf -> Leaf
			| Node (Red, x, l, r) ->
				Node (-1, x, l, r)
			| Node (Black, x, l, r) ->
				Node (1, x, l, r)

		and delete_point = function
			| Leaf ->
				raise Not_found
			| Node (balance, x', l, r)
				if x < 'x
					pass
				else if x > x' then
					pass
				else
					match (balance, l, r) with
						| (_, Leaf, branch)
						| (_, branch, Leaf) ->
							branch
						|


		and balance: integer bst -> integer bst =
			| Leaf -> Leaf
			| Node (1, z, (Node (-1, y, (Node (-1, x, a, b)), c)), d)
			| Node (1, z, (Node (-1, x, a, (Node (-1, y, b, c)))), d)
			| Node (1, x, a, (Node (-1, z, (Node (-1, y, b, c)), d)))
			| Node (1, x, a, (Node (-1, y, b, (Node (-1, z, c, d))))) ->
				Node (-1, y, (Node (1, x, a, b)), (Node (1, z, c, d)))
			| 

		union

























end