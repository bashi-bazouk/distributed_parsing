open Printf
open Stream
open StreamParsing


module CharacterSet =
	functor (S: sig
		val cardinality: int
	end) -> struct

	type t = {
		complemented: bool;
		mutable points: int list;
		mutable intervals: (int * int) list }

	let empty = {
		complemented = false;
		points = [];
		intervals = [];
	}

	let point p = { empty with points = [p] }

	let interval ivl = { empty with intervals = [ivl] }


	let push s (x,y) =
		if y < x then
			failwith "Invalid interval";
		
		if y = x then
			s.points <- y::s.points
		else
			s.intervals <- (x,y)::s.intervals


	let simplify s: t =
		let all_intervals = s.intervals@(List.map (fun p -> (p,p)) s.points) in
		let sorted_intervals = List.sort (fun (x0,_) (x1,_) -> x0 - x1) all_intervals in

		let rec extend s = function
			| [] -> s
			| (first_x, first_y)::tl ->
				let (last_x, last_y) =
					List.fold_left (fun (current_x, current_y) (next_x, next_y) ->
						if (next_x - current_y) > 1 then
							(* Intervals are disjoint. Push the current interval and start a new interval.*)
							(push s (current_x, current_y);
							(next_x, (min S.cardinality next_y)))
						else
							(* Intervals abut, or are overlapping. Extend the current interval. *)
							(current_x, (min S.cardinality next_y))
						)
						(max 0 first_x, min S.cardinality first_y) 
						tl in

					if ((last_y = last_x) && (not (List.mem last_y s.points))) || 
						((last_y != last_x) && (not (List.mem (last_x, last_y) s.intervals))) then
						push s (last_x, last_y);
					
					s in

		extend { empty with complemented = s.complemented } sorted_intervals


	let complement s =
		simplify {
			complemented = not s.complemented;
			points = [];
			intervals =
				let all_intervals = s.intervals@(List.map (fun p -> (p,p)) s.points) in
				let left_complements =
					List.map 
						(fun (x,_) -> (0,x-1))
						(List.filter (fun (x,_) -> x > 0) all_intervals)
				and right_complements =
					List.map 
						(fun (_,y) -> (y+1, S.cardinality))
						(List.filter (fun (_,y) -> y < S.cardinality) all_intervals) in
				left_complements@right_complements
			}


	let rec combine s s' =
		if s.complemented != s'.complemented then
			combine s (complement s')
		else
			simplify { s with
				points = s.points@s'.points;
				intervals = s.intervals@s'.intervals }


	let cardinality s =
		let s = simplify s in
		let count = 
			List.fold_left (+) (List.length s.points) (List.map (fun (x,y) -> (y-x+1)) s.intervals) in
		if not s.complemented then
			count
		else
		S.cardinality - count



	let is_point s =
		(cardinality s) = 1


end




module ASCII = struct
	
	include CharacterSet(struct let cardinality = 128 end)
	type character_set = t


	let read_escape_character (input: char Stream.t): int =
		junk input;
		match assert_one input "Expected an escape character." with
			| 'X' | 'x' -> 
				junk input;
				int_of_string ("0x"^(assert_n 2 input) "expected a hex byte")
			| 'O' | 'o' -> 
				junk input; 
				int_of_string ("0o"^(assert_n 3 input) "expected an octal byte")
			| 'B' | 'b' -> 
				junk input;
				int_of_string ("0b"^(assert_n 8 input) "expected a binary byte")
			| 't' ->
				junk input;
				int_of_char '\t'
			| 'n' ->
				junk input;
				int_of_char '\n'
			| 'r' ->
				junk input;
				int_of_char '\r'
			| 'v' ->
				junk input;
				11
			| 'f' ->
				junk input;
				12 
			| c -> fail ~error:(Printf.sprintf "Invalid escape character \\%c" c) input


	let read_point (input: char Stream.t): int =
		match assert_one input "Expected a character." with
			| '\\' ->	read_escape_character input
			| c ->
				junk input; int_of_char c



	let rec read (input: char Stream.t): character_set =
		(* Read a character set. *)

		junk input; (* Junk the '[' *)

		(* We are going to return character_set. *)
		let character_set = ref empty in

		let merge character_set' =
			(* merge c0 c1 adds the points and intervals in c1 to c0. *)
			(* To keep things simple, this op ignores complementation. *)
			(* This allows us to initially set complementation, and then propagate that value. *)
			character_set := 
				combine 
					!character_set 
					{ character_set' with complemented = !character_set.complemented } in


		(* Check for complementation. *)
		(match peek input with
			| Some '^' ->
				junk input;
				character_set := { !character_set with complemented = true }
			| _ -> ()
		);

		(* Read macros, points, and intervals *)
		let not_done = ref true in
		while !not_done do
			let patch = read_macro_or_point_or_interval input in
			merge patch;

			not_done := 
				match peek input with
					| Some ']' -> false
					| Some _ -> true
					| None ->
						fail ~error:"Expected ]" input
		done;


		junk input; (* Junk the ']' *)
		!character_set

	(* End of read *)

	and maybe_read_macro (input: char Stream.t): character_set option =
		guard_by_prefix input [
			("[:alnum:]", fun (input) -> 
				njunk input 9; 
				read (of_string "[A-Za-z0-9]"));
			("[:word:]", fun (input) -> 
				njunk input 8; 
				read (of_string "[A-Za-z0-9_]"));
			("[:alpha:]", fun (input) -> 
				njunk input 9; 
				read (of_string "[A-Za-z]"));
			("[:blank:]", fun (input) -> 
				njunk input 9; 
				read (of_string "[ \t]"));
			("[:cntrl:]", fun (input) -> 
				njunk input 9; 
				read (of_string "[\x00-\x01F\x7F]"));
			("[:digit:]", fun (input) -> 
				njunk input 9; 
				read (of_string "[0-9]"));
			("[:graph:]", fun (input) -> 
				njunk input 9; 
				read (of_string "[\x21-\x7E]"));
			("[:lower:]", fun (input) -> 
				njunk input 9; 
				read (of_string "[a-z]"));
			("[:print:]", fun (input) -> 
				njunk input 9; 
				read (of_string "[\x20-\x7E]"));
			("[:punct:]", fun (input) -> 
				njunk input 9; 
				read (of_string "[][!\"#$%&'(input)*+,./:;<=>?@\\^_`{|}~-]"));
			("[:space:]", fun (input) -> 
				njunk input 9; 
				read (of_string "[ \t\r\n\\v\\f]"));
			("[:upper:]", fun (input) -> 
				njunk input 9; 
				read (of_string "[A-Z]"));
			("[:xdigit:]", fun (input) -> 
				njunk input 10; 
				read (of_string "[A-Fa-f0-9]"));
			("\\w", fun (input) ->
				junk input;
				read (of_string "[A-Za-z0-9_]"));
			("\\W", fun (input) ->
				junk input;
				read (of_string "[^A-Za-z0-9_]"));
			("\\a", fun (input) ->
				junk input;
				read (of_string "[A-Za-z]"));
			("\\s", fun (input) ->
				junk input;
				read (of_string "[ \\t]"));
			("\\d", fun (input) ->
				junk input;
				read (of_string "[0-9]"));
			("\\D", fun (input) ->
				junk input;
				read (of_string "[^0-9]"));
			("\\l", fun (input) ->
				junk input;
				read (of_string "[a-z]"));
			("\\p", fun (input) ->
				junk input;
				read (of_string "[\\x20-\\x7E]"));
			("\\s", fun (input) ->
				junk input;
				read (of_string "[ \t\r\n\\v\\f]"));
			("\\S", fun (input) ->
				junk input;
				read (of_string "[^ \t\r\n\\v\\f]"));
			("\\u", fun (input) ->
				junk input;
				read (of_string "[A-Z]"));
			("\\x", fun (input) ->
				junk input;
				read (of_string "[A-Fa-f0-9]"))]


	and read_macro_or_point_or_interval (input: char Stream.t): character_set =
		match maybe_read_macro input with
			| Some cs -> cs
			| None ->
				let first_point = read_point input in
				(match peek input with
					| Some '-' ->
						if (npeek 2 input) <> ['-'; ']'] then (
							junk input;
							let second_point = read_point input in
							interval (first_point, second_point)
						) else
							point first_point
					| _ ->
						point first_point
				)


	let show ?escaped:(escaped=[]) cs =
		if (is_point cs) && (not cs.complemented)then
			try 
				let point = List.hd cs.points in
				(match char_of_int point with
						| '\n' -> "\\n"
						| '\t' -> "\\t"
						| c ->
							if List.mem point escaped then
								Printf.sprintf "\\%c" c
							else
								String.make 1 c
							)
				with _ ->
					let show_range (x,y) =
						sprintf "%c-%c" (char_of_int x) (char_of_int y) in
					printf "\n\nWe got a live one! %s\n\n" (String.concat "," (List.map show_range cs.intervals));
					"[dang]"
		else

			let buf = Buffer.create 16 in

			Buffer.add_char buf '[';

			if cs.complemented then (
				Buffer.add_char buf '^');

			if List.mem 93 cs.points then
				Buffer.add_char buf ']';

			List.iter (fun i -> 
				if i != 45 && i != 93 then
					Buffer.add_char buf (char_of_int i)) cs.points;

			List.iter (fun (x,y) ->
				Buffer.add_char buf (char_of_int x);
				Buffer.add_char buf  '-';
				Buffer.add_char buf (char_of_int y)) cs.intervals;


			if List.mem 45 cs.points then
				Buffer.add_char buf '-';

			Buffer.add_char buf ']';

			Buffer.contents buf


end




























