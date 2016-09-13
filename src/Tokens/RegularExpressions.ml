open List
open Stream
open StreamParsing
open CharacterSets

let sprintf = Printf.sprintf


module Enhanced = struct

	module CharacterSet = CharacterSets.ASCII
	type character_set = CharacterSets.ASCII.t

	type primitive = 
		| Start
		| End
		| CharacterSet of character_set

	type t =
		| Accept
		| Primitive of primitive
		| Group of t
		| Iterate of t * int * (int option)
		| Conjunction of t * t
		| Disjunction of t * t



	(* Regular expression parsing! *)

	let read_escape_character (input: char Stream.t) =
		(* Reads a top-level escape character from a regular eession string. *)
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
		(* Reads a single sub-eession from a regular eession string. *)
		let base = 
			(match assert_one input "End of Input" with
				| '^' -> 
					junk input;
					Primitive(Start)
				| '$' ->
					junk input;
					Primitive(End)
				| '.' ->
					junk input;
					Primitive(CharacterSet(CharacterSet.wildcard))
				| '(' ->
					junk input;
					let group = Group (read ~nesting:(nesting+1) input) in
					junk input;
					group
				| '\\' ->
					Primitive(read_escape_character input)
				| '[' ->
					Primitive(CharacterSet(CharacterSet.read input))
				| c ->
					Primitive(CharacterSet(CharacterSet.point (CharacterSet.read_point input)))
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


	and read ?nesting:(nesting=0) ?prev:(prev=Accept) (input: char Stream.t): t =
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
						let next = read ~nesting:nesting ~prev:Accept input in
						read ~nesting:nesting ~prev:(Disjunction(prev, next)) input
					| _ ->
						let next = read_one ~nesting:nesting input in
						(match prev with
							| Accept ->
								read ~nesting:nesting ~prev:next input
							| _ ->
								read ~nesting:nesting ~prev:(Conjunction(prev, next)) input)
				)


	let rec show = function
		| Accept -> ""
		| Primitive(Start) -> "^"
		| Primitive(End) -> "$"
		| Primitive(CharacterSet(cs)) ->
			if cs = CharacterSet.wildcard then
				"."
			else
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
			| Accept ->
				print_string "Accept";
			| Primitive(Start) ->
				print_string "Start";
			| Primitive(End) ->
				print_string "End";
			| Primitive(CharacterSet(cs)) ->
				if cs = CharacterSet.wildcard then
					print_string "Wildcard"
				else
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


	(* Analysis *)

	let rec nondeterministic_step: t -> t list = function
		| Accept ->
			[ ]

		| Primitive(_) ->
			[Accept]

		| Conjunction(Primitive(_), regexp) ->
			[regexp]

		| Conjunction(Accept, regexp) ->
			nondeterministic_step regexp

		| Conjunction(regexp, regexp') ->
			let substeps = nondeterministic_step regexp in
			map (fun regexp'' -> Conjunction(regexp'', regexp')) substeps

		| Disjunction(left, right) ->
			(nondeterministic_step left)@(nondeterministic_step right)

		| Iterate(Accept, _, _)
		| Iterate(_, 0, Some 0) -> [Accept]
		| Iterate(regexp, 0, Some max) ->
			[Accept]@(nondeterministic_step (Conjunction(regexp, Iterate(regexp, 0, Some (max-1)))))

		| Iterate(regexp, min, Some max) ->
			nondeterministic_step (Conjunction(regexp, Iterate(regexp, min-1, Some(max-1))))

		| Iterate(regexp, min, None) ->
			nondeterministic_step (Conjunction(regexp, Iterate(regexp, min-1, None)))

		| Group(regexp) ->
			let substeps =  nondeterministic_step regexp in
			map (fun regexp' -> Group(regexp')) substeps


	let rec nondeterministic_bfs
		?visitor:(visitor=fun _ -> ()) 
		?terminate:(terminate=fun _ -> false)
		: t list -> unit = function
		| [] -> ()
		| hd::tl ->
			if terminate hd then
				nondeterministic_bfs tl
			else
				(visitor hd;
				nondeterministic_bfs (tl@(nondeterministic_step hd)))

	let breadth_first_search ?visitor ?terminate (regexp: t): unit =
		nondeterministic_bfs ?visitor:visitor ?terminate:terminate [regexp]


	(* Actions *)

	type transition = t * t
	type action = primitive option * transition
	type actions = (primitive option, transition) Hashtbl.t

	let rec actions ~table:(table: (primitive option, t * t) Hashtbl.t) (regexp: t): actions =
		let declare_transition ?action target =
			Hashtbl.add table action (regexp, target) in
		(match regexp with
			| Accept -> ()

			| Primitive(primitive) -> declare_transition ~action:primitive Accept

			| Group(regexp') -> declare_transition regexp'

			| Iterate(Accept, _, _)
			| Iterate(_, _, Some 0) -> 
				declare_transition Accept

			| Iterate(regexp', 0, None) ->
				declare_transition (Conjunction(regexp', Iterate(regexp', 0, None)));
				declare_transition Accept

			| Iterate(regexp', min, None) ->
				declare_transition (Conjunction(regexp', Iterate(regexp', min-1, None)))

			| Iterate(regexp', 0, Some max) ->
				declare_transition (Conjunction(regexp', Iterate(regexp', 0, Some (max-1))));
				declare_transition Accept

			| Iterate(regexp', min, Some max) ->
				declare_transition (Conjunction(regexp', Iterate(regexp', min-1, Some (max-1))))

			| Conjunction(Accept, regexp') ->
				declare_transition regexp'

			| Conjunction(regexp', regexp'') ->
				let subactions = actions ~table:(Hashtbl.create 8) regexp' in
				Hashtbl.iter (fun a (_, reduced) -> 
					declare_transition 
						?action:a 
						(Conjunction(reduced, regexp''))
				) subactions

			| Disjunction(regexp', regexp'') ->
				declare_transition regexp';
				declare_transition regexp''
		);
		table


	module Analyzer = struct

		(* An analyzer is a character-stream processor, that takes a regular expression as a parameter.
		 * The analyzer treats the character-stream directly.
		 * The analyzer can be 
		 *   a) reset to an initial state (all ones), and
		 *   b) run deterministically or nondeterministically
		 * The analyzer will
		 *   - output the boundaries of complete matches.
		 *   - track and output the boundaries of incomplete matches
		 *   - track the presence of grouped reductions, and pass-thru captured text.

		*)

		type analyzer = {
			cardinality: int;
			alphabet: t array;
			reverse_index: (t, int) Hashtbl.t;
			transitions: (int option, int) Hashtbl.t;

			cursor: int array;
		}

		let load (regexp: t): analyzer =
			pass

		let reset (analyzer: analyzer) =
			Array.fill analyzer.cursor 0 analyzer.cardinality 1


		let state_machine_of_regexp (regexp: t) =
			let cardinality = ref 0
			and reverse_index = Hashtbl.create 64
			and transitions = Hashtbl.create 256 in
			breadth_first_search
				~visitor:(fun regexp ->
					Hashtbl.add reverse_index regexp !cardinality;
					incr cardinality;
					List.iter (fun next -> 
						try
							Hashtbl.add transitions (!cardinality-1) (Hashtbl.find reverse_index next)
						with Not_found ->
							(Hashtbl.add reverse_index next !cardinality;
							Hashtbl.add transitions (!cardinality-1) !cardinality;
							incr cardinality)
					) (nondeterministic_step regexp))
				~terminate:(fun regexp ->
					try
						Hashtbl.find_all transitions (Hashtbl.find reverse_index regexp) > 0
					with Not_found ->
						false)
				regexp;
			let alphabet = Array.make cardinality Accept in
			Hashtbl.iter (fun (regexp, i) -> Array.set alphabet i regexp) reverse_index;
			{ cardinality= !cardinality;
				alphabet;
				reverse_index;
				transitions }

		type cursor = int array

		let init (state_machine: state_machine): cursor = Array.make state_machine.cardinality 1
		let term (state_machine: state_machine): cursor = Array.make state_machine.cardinality 0


		let step ?cursor' (state_machine: state_machine) (cursor: cursor): unit =
			let cursor' = Array.make state_machine.cardinality 0 in
			let updates =
				Array.mapi
					(fun multiplicity i ->
						if multiplicity > 0 then
							fun () ->
								List.iter
									(fun next -> cursor'.(next) <- cursor'.(next) + multiplicity)
									(Hashtbl.find_all state_machine.transitions i)
						else
							fun _ -> ())
					cursor in
			
			Array.iter (fun update -> update(cursor')) updates;
			cursor'

	end
(*


	let index (regexp: t): (t, int) Hashtbl =
		let cardinality = ref 0
		and table = Hashtbl.create 64 in
		nondeterministic_bfs
			~visitor:(fun regexp ->
				Hashtbl.add table regexp !cardinality;
				incr cardinality)
			~terminate:(Hashtbl.mem table)
			regexp;
		table


	let transitions (regexp: t): (int -> int) Hashtbl =
		let cardinality = ref 0
		and table = Hashtbl.create 64 in
		nondeterministic_bfs
			~visitor:(fun regexp ->



	type analysis = {
		mutable index: t list;
		transitions: (t, t) Hashtbl;
	}

	let analyze (regexp: t) =
		let bfs 
		


	let rec destructure: t -> (t * t) list = function
		(* breaks a regular expression into a (head, tail) structure, by breaking a conjunction. *)
		| Accept -> [(Accept,[])]
		| Primitive(_) as p -> [(p,[])]
		| Group x -> destructure x
		| Conjunction(Accept, tail) ->
			destructure tail
		| Conjunction(e, e') ->
			let cases = destructure e in
			map (function
				| (hd, Accept) ->
					(hd, e')
				| (hd, tl) ->
					(hd, Conjunction(tl, e'))) cases
		| Iterate(e, min, Some max) ->
			if max > 1 then
				let cases = destructure e in
				map (fun (hd,tl) -> (hd, Conjunction(tl, Iterate(e, min-1, Some(max-1))))) cases
			else
				if max = 1 then
					let cases = destructure e in
					map (fun (hd,tl) -> (hd, Accept)) cases
				else
					[]
		| Iterate(e, min, None) ->
				let cases = destructure e in
				map (fun (hd,tl) -> (hd, Conjunction(tl, Iterate(e, min-1, None)))) cases
		| Disjunction(a,b) ->
			(destructure a)@(destructure b)

	let iteri ?offset:(offset=0) callback: t -> int = function
		| Accept -> ()

		| Primitive(_) -> ()



	let rec project_primitives: t -> primitive list = function
		| Accept -> []
		| Primitive(c) -> [c]
		| Group(g) -> project_primitives(g)
		| Iterate(i,_,_) ->
			project_primitives(i)
		| Conjunction(l,r) ->
			(project_primitives l)@(project_primitives r)
		| Disjunction(l,r) ->
			(project_primitives l)@(project_primitives r)


	let rec project_transitions ?offset:(offset=0) declare = function
		| Accept -> ()
		| 


	module Index = struct

		type basis = {
			regexp: t;
			count: int;
			subbases: (int * basis) list;
		}
		
		type expanded_coordinates = (int * int) list
		type coordinates = int list
		type index = int

		let rec generate_basis (regexp: t) =
			let rec flat_fold ?count:(count=0) ?subbases:(subbases=[]) = function
				| Primitive(Start)
				| Primitive(End)
				| Accept as regexp -> { regexp; count; subbases }
				| Primitive(CharacterSet(_)) ->
					{ regexp; count=(count+1); subbases }
				| Group(x) ->
					flat_fold ~count:count ~subbases:subbases x
				| Iterate(x, min, maybe_max) ->
					let subbasis = generate_basis x in
					{ regexp; count; subbases=subbases@[(count, subbasis)] }
				| Conjunction(l,r) ->
					let basis' = flat_fold ~count:count ~subbases:subbases l in
					flat_fold ~count:basis'.count ~subbases:basis'.subbases r
				| Disjunction(l,r) ->
					let subbasis_0 = generate_basis l
					and subbasis_1 = generate_basis r in
					{ regexp; count; subbases=subbases@[(count, subbasis_0); (count, subbasis_1)] } in
			flat_fold regexp

		let project_transitions (regexp: t): (int list) array =
			let basis = generate_basis regexp in
			let transitions = Array.make (


		let rec flatten_basis basis =
			let rec fold_basis r_prefix (_, basis) =
				fold_left fold_basis (basis.count::r_prefix) basis.subbases in
			rev (fold_basis [] (0, basis))

		let expand_coordinates c basis =
			combine c (flatten_basis basis)


		let rec unique_indices (basis: basis): int =
			fold_left (+) basis.count (map unique_indices (map snd basis.subbases))


		let rec coordinates_of_index i basis =
			match basis.subbases with
				| [] ->
					if i >= basis.count then
						raise Not_found
					else
						[i]
				| (start, subbasis)::rest ->
					if i <= start then
						i::(map (fun _ -> 0) basis.subbases)
					else
						try
							start::(coordinates_of_index (i-start) subbasis)
						with
							Not_found ->
								coordinates_of_index i { basis with subbases=rest }

		let rec cardinal: t -> t = function
			(* cardinal regexp returns a regular expression, where 
			 * Iterations and Disjunctions have been mapped to Conjunctions. *)
			| Group(x) -> Group (cardinal x)
			| Conjunction(l,r)
			| Disjunction(l,r) -> Conjunction(cardinal l, cardinal r)
			| Iterate(e, minimum, _) ->
				let minimum = min 1 minimum in
				let iterations = Array.to_list (Array.make minimum e) in
				fold_left (fun acc x -> Conjunction(acc, x)) (hd iterations) (tl iterations)
			| default -> default


		let rec cardinality: t -> int = function
			(* cardinality e is (essentially) the (printed) size of cardinal e. *)
			| Accept -> 0
			| Primitive(_) -> 1
			| Group(x) -> cardinality x
			| Iterate(_,_,_) as i -> cardinality (cardinal i)
			| Conjunction(x,y)
			| Disjunction(x,y) -> (cardinality x) + (cardinality y)

	end


	module Matcher = struct
		open Index

		type matching = {
			primitives: primitive array;
			transitions: int -> int list;
			multiplicities: int array;
		}
(*
		let build_from_regexp (regexp: t): t =
			let basis = generate_basis regexp in
			let 
*)

	end

*)
end









