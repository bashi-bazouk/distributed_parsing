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


	let sequents (grammar: ('t, 'nt) grammar): ('t list) * ('nt list) =
		let maybe_add s ss = if mem s ss then ss else ss@[s] in
		fold (fun nt sequents (ts, nts) ->
			let nts = maybe_add nt nts in
			fold_left (function
				| T t -> ((maybe_add t ts), nts)
				| NT nt -> (ts, (maybe_add nt nts))
			) (ts, nts) sequents
		) ([], []) grammar

	let terminals grammar = fst (sequents grammar)
	let nonterminals grammar = snd (sequents grammar)


	(* Reachability *)

	let pushout (grammar: ('t, 'nt) grammar)	(subgrammar: ('t, 'nt) grammar): 'nt list = 
		filter (fun nt -> not (Hashtbl.mem subgrammar nt)) (nonterminals subgrammar)
		map (fun nt -> nt := (grammar ?? nt)) (nonterminals subgrammar)


	let cocover (grammar: ('t, 'nt) grammar)	(subgrammar: ('t, 'nt) grammar): ('t, 'nt) grammar =
		union grammar::(map (fun nt -> nt := (grammar ?? nt)) (pushout grammar subgrammar))


	let rec reachable (grammar: ('t, 'nt) grammar) (subgrammar: ('t, 'nt) sequence): ('t, 'nt) grammar =
		least_fixpoint (cocover grammar) subgrammar


	(* Co-Reachability *)

	let pullback (grammar: ('t, 'nt) grammar)	(subgrammar: ('t, 'nt) grammar): 'nt list = 
		pass

	let cover (grammar: ('t, 'nt) grammar)	(subgrammar: ('t, 'nt) grammar): ('t, 'nt) grammar = 
		pass

	let rec coreachable (final: 'nt) (grammar: ('t, 'nt) grammar): ('t, 'nt) grammar =

		let nonterminal_closure sequents = 
			fold (fun nonterminal disjunction grammar' ->
				let coreachable_disjunction = filter (List.mem sequents) disjunction in
				if (length coreachable_disjunction) = 0 then
					grammar'
				else
					grammar' ++ (nonterminal := coreachable_disjunction)
			) empty_grammar grammar in

		let at_fixpoint = ref false in
		let grammar' = ref (nonterminal_closure [final]) in
		let nonterminal_sequents = ref (map (fun nt -> NT nt) (sequents !grammar'))

		while not !at_fixpoint do

			let closure = nonterminal_closure nonterminal_sequents in

			let grammar'' = !grammar' ++ closure in

			let nonterminal_sequents' = sequents grammar'' in

			if (length nonterminal_sequents') = (length !nonterminal_sequents) then
				at_fixpoint := true			
			else
				grammar' := !grammar' ++ grammar'';
				nonterminal_sequents := nonterminal_sequents';

		done

		!grammar'

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

				val (s, sigma) = sequents grammar
				val parents = map self#lookup (coreachable

				method lookup sequent: ['t, 'nt] sentinel =
					find sentinels 


				method measure (start, length): float = 0.0

			end














end