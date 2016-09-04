open List
open Stream
open StreamParsing
open CharacterSets

let sprintf = Printf.sprintf


module Enhanced = struct

	module CharacterSet = CharacterSets.ASCII
	type character_set = CharacterSets.ASCII.t

	type t =
		| Nil | Start | End | Wildcard
		| CharacterSet of character_set
		| Group of t
		| Iterate of t * int * (int option)
		| Conjunction of t * t
		| Disjunction of t * t



	(* Regular expression parsing! *)

	let read_escape_character (input: char Stream.t) =
		(* Reads a top-level escape character from a regular expression string. *)
		junk input;
		match assert_one input "Expected an escape character" with
			| '\\' ->
				junk input;
				CharacterSet (CharacterSet.point (int_of_char '\\'))
			| 'n' ->
				junk input;
				CharacterSet (CharacterSet.point (int_of_char '\n'))
			| 't' ->
				junk input;
				CharacterSet (CharacterSet.point (int_of_char '\t'))
			| '^' ->
				junk input;
				CharacterSet (CharacterSet.point (int_of_char '^'))
			| '$' ->
				junk input;
				CharacterSet (CharacterSet.point (int_of_char '$'))
			| '.' ->
				junk input;
				CharacterSet (CharacterSet.point (int_of_char '.'))
			| '*' ->
				junk input;
				CharacterSet (CharacterSet.point (int_of_char '*'))
			| '?' ->
				junk input;
				CharacterSet (CharacterSet.point (int_of_char '?'))
			| '+' ->
				junk input;
				CharacterSet (CharacterSet.point (int_of_char '+'))
			| '(' ->
				junk input;
				CharacterSet (CharacterSet.point (int_of_char '('))
			| ')' ->
				junk input;
				CharacterSet (CharacterSet.point (int_of_char ')'))
			| '[' ->
				junk input;
				CharacterSet (CharacterSet.point (int_of_char '['))
			| ']' ->
				junk input;
				CharacterSet (CharacterSet.point (int_of_char ']'))
			| '{' ->
				junk input;
				CharacterSet (CharacterSet.point (int_of_char '{'))
			| '}' ->
				junk input;
				CharacterSet (CharacterSet.point (int_of_char '}'))
			| c -> 
				fail ~error:(sprintf "Invalid escape character \\%c" c) input


	let rec read_one ?nesting:(nesting=0) (input: char Stream.t): t =
		(* Reads a single sub-expression from a regular expression string. *)
		let base = 
			(match assert_one input "End of Input" with
				| '^' -> 
					junk input;
					Start
				| '$' ->
					junk input;
					End
				| '.' ->
					junk input;
					Wildcard
				| '(' ->
					junk input;
					let group = Group (read ~nesting:(nesting+1) input) in
					junk input;
					group
				| '\\' ->
					read_escape_character input
				| '[' ->
					CharacterSet (CharacterSet.read input)
				| c ->
					CharacterSet (CharacterSet.point (CharacterSet.read_point input))
					) in
		match peek input with
			| Some '*' ->
				junk input;
				Iterate(base, 0, None)
			| Some '?' ->
				junk input;
				Iterate(base, 0, Some 1)
			| Some '+' ->
				junk input;
				Iterate(base, 1, None)
			| Some '{' ->
				junk input;
				let min = take_decimal input in
				drop_whitespace input;
				(match peek input with
					| Some ',' ->
						junk input;
						(match assert_one input "Expected int or }" with
							| '}' ->
								junk input;
								Iterate(base, min, None)
							| _ ->
								let max = take_decimal input in
								drop_whitespace input;
								(match peek input with
									| Some '}' -> 
										junk input;
										Iterate(base, min, Some max)
									| _ -> fail ~error:"Expected }" input)
						)
					| Some '}' ->
						junk input;
						Iterate(base, min, Some min)
					| _ -> fail ~error:"Expected , or }" input)
			| _ ->
				base


	and read ?nesting:(nesting=0) ?prev:(prev=Nil) (input: char Stream.t): t =
		match peek input with
			| None -> 
				if nesting > 1 then
					fail ~error:"Unmatched (" input
				else
					prev
			| Some c ->
				(match c with
					| ')' ->
						if nesting < 1 then
							fail ~error:"Unmatched )" input
						else
							prev
					| '|' ->
						junk input;
						let next = read ~nesting:nesting ~prev:Nil input in
						read ~nesting:nesting ~prev:(Disjunction(prev, next)) input
					| _ ->
						let next = read_one ~nesting:nesting input in
						(match prev with
							| Nil ->
								read ~nesting:nesting ~prev:next input
							| _ ->
								read ~nesting:nesting ~prev:(Conjunction(prev, next)) input)
				)









(*
	let rec read ?acc:(r_conjunction=[]) (input: char Stream.t): t =
		(* Reads a regular expression from input *)
		match peek input with
			| None -> (conjugate (rev r_conjunction))
			| Some c ->
				(match c with
					| ' ' | '\t' -> 
						junk input;
						read ~acc:r_conjunction input
					| '^' -> 
						junk input;
						read ~acc:(Start::r_conjunction) input
					| '$' ->
						junk input;
						read ~acc:(End::r_conjunction) input
					| '.' ->
						junk input;
						read ~acc:(Wildcard::r_conjunction) input
					| '*' ->
						(match r_conjunction with
							| last::initial ->
								junk input;
								read ~acc:(Iterate(last, 0, None)::initial) input
							| [] -> fail input)
					| '?' ->
						(match r_conjunction with
							| last::initial ->
								junk input;
								read ~acc:(Iterate(last, 0, Some 1)::initial) input
							| [] -> fail input)
					| '+' ->
						(match r_conjunction with
							| last::initial ->
								junk input;
								read ~acc:(Iterate(last, 0, None)::initial) input
							| [] -> fail input)
					| '(' ->
						read ~acc:((read_group input)::r_conjunction) input
					| '\\' ->
						read ~acc:((read_escape_character input)::r_conjunction) input
					| '[' ->
						read ~acc:((CharacterSet (CharacterSet.read input))::r_conjunction) input
					| c ->
						read ~acc:((CharacterSet (CharacterSet.point (CharacterSet.read_point input)))::r_conjunction) input
				)
						


	and read_group (input: char Stream.t) =
		(* Reads a grouped regular expression from input *)
		let current_column = count input in
		junk input;
		let nesting = ref 1
		and r_buffer = ref [] in
		while not (((peek input) = Some ')') && (!nesting = 1)) do
			match assert_one input (sprintf "unmatched ( at %i" current_column) with
				| '\\' ->
					junk input;
					r_buffer := (next input)::'\\'::!r_buffer
				| ')' ->
					decr nesting;
					r_buffer := (next input)::!r_buffer
				| '(' ->
					incr nesting;
					r_buffer := (next input)::!r_buffer
				| c -> 
					r_buffer := (next input)::!r_buffer
		done;
		Printf.printf "Got sub-regex %s\n" (String.init (length !r_buffer)  (fun i -> nth (rev !r_buffer) i));
		junk input;
		read (Stream.of_list (rev !r_buffer))
*)


	let rec show = function
		| Nil -> ""
		| Start -> "^"
		| End -> "$"
		| Wildcard -> "."
		| CharacterSet cs ->
			CharacterSet.show ~escaped:(map int_of_char ['\\'; '^'; '$'; '('; ')'; '{'; '}'; '['; ']'; '*'; '+'; '?'; '|']) cs
		| Group r ->
			"(" ^ (show r) ^ ")"
		| Iterate (r, min, maybe_max) ->
			(match (min, maybe_max) with
				| (0, None) ->
					(show r) ^ "*"
				| (1, None) ->
					(show r) ^ "+"
				| (0, Some 1) ->
					(show r) ^ "?"
				| (_, None) ->
					sprintf "%s{%s,}" (show r) (string_of_int min)
				| (_, Some max) ->
					if min = max then
						sprintf "%s{%i}" (show r) min
					else
						sprintf "%s{%i,%i}" (show r) min max)
		| Conjunction (l,r) ->
			(show l)^(show r)
		| Disjunction (l,r) ->
			(show l)^"|"^(show r)


	open Format

	let rec print (r: t) =
		let group_count = ref 1 in
		let rec print = function
			| Nil ->
				print_string "Nil";
			| Start ->
				print_string "Start";
			| End ->
				print_string "End";
			| Wildcard ->
				print_string "Wildcard";
			| CharacterSet cs ->
				print_string (CharacterSet.show cs);
			| Group r ->
				printf "Group %d (" !group_count;
				incr group_count;
				open_vbox 2;
				print_break 1 2;
				print r;
				print_string ")";
				print_break 1 2;
				close_box();
			| Iterate (r, min, maybe_max) ->
				(match (min, maybe_max) with 
					| (0, None) -> printf "(Iterate 0 ∞"
					| (_, None) -> printf "(Iterate %d ∞" min
					| (_, Some max) -> printf "(Iterate %d %d" min max);
				open_vbox 2;
				print_space();
				print r;
				print_string ")";
				close_box();
				force_newline();
			| Conjunction (l,r) ->
				print_string "(Conjunction";
				print_break 0 2;
				open_vbox 0;
				print l;
				force_newline();
				print r;
				print_string ")";
				close_box();
				force_newline();
			| Disjunction (l,r) ->
				print_string "(Disjunction";
				open_vbox 2;
				print_break 1 2;
				print l;
				force_newline();
				print r;
				print_string ")";
				close_box();
				force_newline() in
		print r





	(* Analysis*)

	let rec destructure: t -> (t * t) list = function
		(* breaks a regexp into a (head, tail) structure, by breaking a conjunction. *)
		| Nil
		| Start
		| End 
		| Wildcard
		| CharacterSet _ -> []
		| Group x -> destructure x
		| Conjunction(Nil, tail) ->
			destructure tail
		| Conjunction(a, b) ->
			let cases = destructure a in
			map (fun (hd,tl) -> (hd, Conjunction(tl, b))) cases
		| Iterate(regexp, min, Some max) ->
			if max > 1 then
				let cases = destructure regexp in
				map (fun (hd,tl) -> (hd, Conjunction(tl, Iterate(regexp, min-1, Some(max-1))))) cases
			else
				if max = 1 then
					let cases = destructure regexp in
					map (fun (hd,tl) -> (hd, Nil)) cases
				else
					[]
		| Iterate(regexp, min, None) ->
				let cases = destructure regexp in
				map (fun (hd,tl) -> (hd, Conjunction(tl, Iterate(regexp, min-1, None)))) cases
		| Disjunction(a,b) ->
			(destructure a)@(destructure b)


	module Index = struct
		(* There are a variety of unique indices. This one indexes from the boundaries of its subgroups.
		 * A unit is the printed width of a `cardinal` regex, which is derived from the target regex.
		 * A regex with n subgroups has 2n indices [i_0; i_1; ...; i_n].
	   * A negative index indicates that a boundary has not yet been crossed.
	   * Inversely, a positive index indicates that a boundary has been crossed.
	   * A positive index that is equal to the cardinality of its subgroup indicates a closing boundary has been crossed.
	   * A positive index that is greater than n times the cardinality of its subgroup indicates n previous iterations.
		 *)

		type boundary = Open | Close


		let rec cardinal: t -> t = function
			(* cardinal regexp returns a regexp which, for any given iteration,
				 iterates at least once, but not more than the minimum number of iterations. *)
			| Group(x) -> Group (cardinal x)
			| Conjunction(l,r) -> Conjunction(cardinal l, cardinal r)
			| Disjunction(l,r) -> Disjunction(cardinal l, cardinal r)
			| Iterate(regexp, minimum, _) ->
				let minimum = min 1 minimum in
				let iterations = Array.to_list (Array.make minimum regexp) in
				fold_left (fun acc x -> Conjunction(acc, x)) (hd iterations) (tl iterations)
			| default -> default
(*
		let rec cardinality: t -> int = function
			(* cardinality regexp is (essentially) the (printed) size of cardinal regexp.*)
			| Nil | Start | End -> 0
			| Wildcard | CharacterSet(_) -> 1
			| Group(x) -> cardinality x
			| Iterate(_,_,_) as i -> cardinality (cardinal i)
			| Conjunction(x,y)
			| Disjunction(x,y) -> (cardinality x) + (cardinality y)

		let basis t: (boundary * int) list =
			let rec accumulate
				?partial:(acc=[(Open, 0)])
				?count:(i=0) 
				= function
					| Nil
					| Start
					| End -> acc@[(Close, i)]
					| Wildcard
					| CharacterSet _ -> acc@[(Close, i+1)]
					| Group(x) ->
						acc@(accumulate ~boundaries:[(Open, i)] ~count:i)
					| Iterate(_,_,_) as i -> 
						accumulate ~partial:acc ~count:i (cardinal i)
					| Conjunction(l,r) ->

*)
	end




end









