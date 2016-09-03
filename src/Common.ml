

module type Eq = sig
	type elt
end


module PSet = functor (S: Eq) -> struct

	type t = S.elt list

	let compare (ls: t) (rs: t) =
		let rec directed_comparison = function
			| ([], []) -> 0
			| ([], _) -> -1
			| (_, []) -> 1
			| (l::ls, r::rs) ->
				(match ((List.mem l (r::rs)), (List.mem r (l::ls))) with
					| (true, true) -> directed_comparison (ls, rs)
					| (true, false) -> -1
					| (false, true) -> 1
					| (false, false) -> (Hashtbl.hash l) - (Hashtbl.hash r)) in
		directed_comparison (ls, rs)

end


module ConcreteSet = functor (S: Eq) -> struct

	module N = PSet(S)
	module P = Set.Make(N)

	type elt = S.elt
	type t = P.t

	let empty = P.empty

	let of_list (elts: elt list) = 
		List.fold_left P.union P.empty (List.map (fun elt -> P.of_list [[elt]]) elts)

	let is_empty = P.is_empty

	let cardinal = P.cardinal

	let subset = P.subset

	let equal = P.equal

	let compare = P.compare

	let diff = P.diff

	let inter = P.inter

	let union = P.union

	let choose t = List.hd (P.choose t)

	let find elt t = List.hd (P.find [elt] t)

	let split elt = P.split [elt]

	let max_elt t = List.hd (P.max_elt t)

	let min_elt t = List.hd (P.min_elt t)

	let elements t = List.map List.hd (P.elements t)

	let partition p = P.partition (fun elt -> p (List.hd elt))

	let filter p = P.filter (fun elt -> p (List.hd elt))

	let exists p = P.exists (fun elt -> p (List.hd elt))

	let for_all p = P.for_all (fun elt -> p (List.hd elt))

	let fold f t acc = P.fold (fun elt acc -> f (List.hd elt) acc) t acc

	let iter f = P.iter (fun elt -> f (List.hd elt))

	let remove elt = P.remove [elt]

	let singleton elt = P.singleton [elt]

	let add elt = P.add [elt]

	let mem elt = P.mem [elt]

end


module Index (S: Eq) (I: Eq) = struct

end


	