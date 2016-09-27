open TokenMap

module Regex = RegularExpressions.Enhanced

module CursorArray = struct
	(* A cursor at index i is actually at the ith reduction index of cursor 
	 * regular expression. *)

	type cursor = {
		substart: Regex.t;
		(* substart is the reduction that the cursor starts on.
		 * For complete matches, this is the same as t.start.
		 * For incomplete matches, this may be a reduction of t.start. *)

		captures: string array;
		(* captures contains the subgroup strings of the ongoing match. *)
	}

	type t = {
		start: Regex.t;
		(* start is the regular expression that this array index is based on. *)

		mutable front_buffer: cursor list array;
		mutable back_buffer: cursor list array;
		(* front_buffer represents the up-to-date cursor data *)
		(* back_buffer is used for computing step transformations *)
		(* When a step transformation is complete, the front buffer
		 * and back buffer are swapped. *)

		step_memo: (int, (Regex.primitive, int)) Hashtbl;
		(* step_memo is the memoization table for the step function. *)
	}

	let data (cursor_array: t) = cursor_array.front_buffer
	(* data ca is a convenient accessor for the primary data in the array. *)

	let reduction_at_index (cursor_array: t) (i: int): Regex.t =
		let counter = ref i
		and result = ref None in
		let stop_searching _=
			if !counter <= 0 then
				true
			else
				(decr counter;
				false) in
		let remember regex =
			result := regex in
		Regex.breadth_first_search
			~visitor:remember
			~terminate:stop_searching
			cursor_array.start;
		match result with
			None ->
				raise Invalid_argument
			Some regex ->
				regex

	let step (cursor_array: t) (character: int): unit =
		let memo = cursor_array.step_memo in

		(* Clear the back buffer. *)
		for i=0 to Array.length cursor_array.back_buffer do
			cursor_array.back_buffer.(i) <- [])
		done;

		(* Compute the transitions in the back buffer *)
		Array.iteri (fun reduction_index cursors ->
			let transitions = 
				try
					Hashtbl.find_all cursor_array.step_memo reduction_index
				with Not_found -> raise Not_found in
			List.iteri 
				(fun (prim, target) ->
					match prim with
						| CharacterSet(cs) ->
							if Regex.CharacterSet.mem character cs then
								let target_cursors = cursor_array.back_buffer.(target) in
								cursor_array.back_buffer.target <- target_cursors@cursors
						| _ -> ())
				transitions)
		front_buffer

		(* Swap the front and back buffers. *)
		(fun front_buffer back_buffer ->
			cursor_array.back_buffer <- front_buffer;
			cursor_array.front_buffer <- back_buffer)
			cursor_array.front_buffer
			cursor_array.back_buffer;

end

module Tokenizer = struct

	type t = {
		token_map: token_map;
		cursors: (string, CursorArray.t) Hashtbl.t;
	}





end