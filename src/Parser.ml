open Scratch.FreeGrammar
open List


class nonterminal_parser ?observers(obs=[]) grammar nonterminal = object
	
	val id = NT nonterminal
	val disjunction = grammar |- nonterminal
	val subpartitions_by_sequent = map (fun s -> Array.make (length s) []) disjunction
	val mutable observers = obs

	method tell (sequent: sequent) (partition: int list) =
		let locations = Hashtbl.create 16 in
		(* Populate locations. *)
		ignore (fold_left (fun (current_sequence, current_sequent) (sequence, subpartitions_array) ->
			ignore (fold_left (fun (current_sequence, current_sequent) (sequent', subpartitions) -> 
				if sequent' == sequent then
					(subpartitions_array.(current_location) <- partition::subpartitions_array.(current_location);
					Hashtbl.add locations current_sequence current_sequent);
				(current_sequence, current_sequent + 1)
			) (current_sequence, 0) (combine sequence (Array.to_list subpartitions_array)));
			(current_sequence+1, 0)
		) (0, 0) (combine disjunction subpartitions_by_sequent));

		(* Find any new partitions that came out of this update. *)
		let new_partitions =
			Hashtbl.fold (fun new_partitions sequence_index updated_sequent_indices ->
				let has_new_subpartition sequence =
					exists (fun loc -> (nth sequence loc) = partition) updated_sequent_indices in
				let new_partitions' = filter has_new_subpartition (self#search sequence_index) in
				new_partitions'@new_partitions
			) [] locations in

		(* Tell observers about new partitions. *)
		iter (fun parent -> 
			iter (fun partition -> 
				parent#tell id partition
			) new_partitions
		) observers


	method search (index_of_sequence: int): int list list =
		(* search n returns any valid partitions for the nth sequence in the disjunction. *)
		let sequence = nth disjunction index_of_sequence
		and subpartitions = Array.to_list (nth subpartitions_by_sequent index_of_sequence) in
		fold_left (fun partial_partitions (sequent, partitions) ->
			()
		) (hd partitions) (combine (tl sequence) (tl partitions)) 



end


class terminal_parser grammar terminal = object(self)

	val id = T terminal
	val parents = []

	method tell ?width:(width=1) (start: int) =
	  iter (fun parent -> parent#tell id [start, start + width]) parents

end

