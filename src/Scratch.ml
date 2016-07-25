open List
open Hashtbl

type ('k,'v) index = ('k,'v) Hashtbl.t

let least_fixpoint ?eq:(eq=(=)) op x =
	let curr = ref x
	and next = ref (op !curr) in
	while not (eq !curr !next) do
		curr := !next;
		next := op !curr;
	done
	!curr


module FreeGrammar = struct

	type ('t, 'nt) sequent =
		| T of 't
		| NT of 'nt

	type ('t, 'nt) sequents = ('t * 'nt) list

	type ('t, 'nt) sequence = ('t, 'nt) sequent list

	type ('t, 'nt) production = 'nt * ('t, 'nt) sequence

	type ('t, 'nt) disjunction = ('t, 'nt) sequence list

	type ('t, 'nt) grammar = ('nt, ('t, 'nt) disjunction) index

	type ('t, 'nt) cogrammar = ('t, ('nt, 't) disjunction) index


	(* Lookups *)

	let empty_grammar = Hashtbl.create 0


	let (??) (grammar: ('t, 'nt) grammar) (nonterminal: 'nt): ('t, 'nt) disjunction =
		try
			find grammar nonterminal
		with Not_found -> []


	(* Constructors *)

	let (:=) (nt: 'nt) (disjunction: ('t, 'nt) disjunction): ('t, 'nt) grammar = 
		let grammar = Hashtbl.create 1 in
		Hashtbl.add grammar nt disjunction;
		grammar


	let (++) g g' =
		let g'' = Hashtbl.create 16
		and union d d' = fold_right (fun s d'' -> if List.mem s d'' then d'' else s::d'') d' d in
		iter (fun nonterminal disjunction -> replace g nt (union (g ?? nt) d)) g';
		g''

	let union = fold_left (++) empty_grammar


	let (--) (grammar: ('t, 'nt) grammar) (subgrammar: ('t, 'nt) grammar): ('t 'nt) grammar =
		fold (fun nonterminal disjunction complement ->
			let complementary_disjunction =
				if not mem nonterminal subgrammar then
					disjunction
				else
					let subgrammar_disjunction = subgrammar ?? nonterminal in
					filter (fun sequence -> not (List.mem sequence subgrammar_disjunction)) disjunction in 
			if (length complementary_disjunction) > 0 then
				complement ++ (nonterminal := complementary_disjunction)
			else
				complement
		) empty_grammar grammar


	let symmetric_difference l r = (l -- r) ++ (r -- L)


	let intersection l r = (union l r) -- (symmetric_difference l r)


	(* Sequents *)

	let sequents_of_grammar (grammar: ('t, 'nt) grammar): ('t list) * ('nt list) =
		let maybe_add s ss = if mem s ss then ss else ss@[s] in
		fold (fun nt sequents (ts, nts) ->
			let nts = maybe_add nt nts in
			fold_left (function
				| T t -> ((maybe_add t ts), nts)
				| NT nt -> (ts, (maybe_add nt nts))
			) (ts, nts) sequents
		) ([], []) grammar

	let terminals grammar = fst (sequents_of_grammar grammar)
	let nonterminals grammar = snd (sequents_of_grammar grammar)


	(* Reachability *)

	let cover = (grammar: ('t, 'nt) grammar)	(subgrammar: ('t, 'nt) grammar): 'nt list =
		let nonterminals = nonterminals subgrammar in
		filter (Hashtbl.mem grammar) nonterminals

	let pushout (grammar: ('t, 'nt) grammar)	(subgrammar: ('t, 'nt) grammar): ('t, 'nt) grammar =
		let nonterminal_cover = cover subgrammar in
		let covering_disjunctions = map ((??) grammar) nonterminal_cover in
		let covering_rules = map (:=) (zip nonterminal_cover covering_disjunctions) in
		let covering_grammar = union covering_rules in
		subgrammar ++ covering_grammar


	let rec reachable (grammar: ('t, 'nt) grammar) (subgrammar: ('t, 'nt) sequence): ('t, 'nt) grammar =
		least_fixpoint (pushout grammar) subgrammar


	(* Co-Reachability *)

	let cocover (grammar: ('t, 'nt) grammar)	(subgrammar: ('t, 'nt) grammar): 'nt list =
		let nonterminals = nonterminals subgrammar in
		let recognized_nonterminal: ('t, 'nt) sequence -> bool = function 
			| T _ -> false
			| NT nt -> List.mem nt nonterminals in
		fold (fun nonterminal disjunction nonterminal_cocover ->
			if any (any recognized_nonterminal) disjunction then
				nonterminal::nonterminal_cocover
			else
				nonterminal_cocover
		)) [] grammar in


	let pullback (grammar: ('t, 'nt) grammar)	(subgrammar: ('t, 'nt) grammar): ('t, 'nt) grammar = 
		let nonterminal_cocover = cocover grammar (nonterminals subgrammar)
		let cocovering_grammar =
			fold (fun nonterminal disjunction cocovering_grammar ->
				let cocovering_disjunction = filter (any recognized_nonterminal) disjunction in
				if (length cocovering_disjunction) > 0 then
					(nonterminal := cocovering_disjunction) ++ cocovering_grammar
				else
					cocovering_grammar
			) empty_grammar grammar in



		

	let rec coreachable (grammar: ('t, 'nt) grammar) (subgrammar: ('t, 'nt) grammar): ('t, 'nt) grammar =
		least_fixpoint (pullback grammar) subgrammar


	let trim (grammar: ('t, 'nt) grammar) (subgrammar: ('t, 'nt) grammar): ('t, 'nt) grammar =
		let reachable = reachable grammar subgrammar
		and coreachable = coreachable grammar subgrammar in
		intersection reachable coreachable

	type 't buffer = <
		method ask: int -> 't
		method tell: int -> 't -> unit >

	class ['t, 'nt] sentinel
		?sentinels:(sentinels=(Hashtbl.create 16))
		(grammar: ('t, 'nt) grammar)
		(buffer: buffer)
		(sequent: ('t, 'nt) sequent) = 

		if Hashtbl.mem sequent sentinels then
			Hashtbl.find sentinels sequent
		else

			object (self)

				val id = sequent
				val disjunction = grammar ?? nt

				val successors = reachable nt 
				val predecessors = coreachable 

				val (s, sigma) = sequents_of_grammar grammar
				val parents = map self#lookup (coreachable

				method lookup sequent: ['t, 'nt] sentinel =
					find sentinels 


				method measure (start, length): float = 0.0

			end














end