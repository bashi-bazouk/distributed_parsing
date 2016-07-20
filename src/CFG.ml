open List
open Printf
open Str


module type T = sig

	type terminal
	type nonterminal

  type sequent =
    | T of terminal
    | NT of nonterminal

  type production = nonterminal * (sequent list)

  type grammar = production list

  val read: string -> grammar

  val show: grammar -> string

  val write: string -> grammar -> unit

  val nonterminals: grammar -> nonterminal list

  val terminals: grammar -> terminal list

end


module StringGrammar: T = struct

	type terminal = string
	type nonterminal = string

  type sequent =
    | T of terminal
    | NT of nonterminal

  type production = nonterminal * (sequent list)

  type grammar = production list


  let read (filename: string): grammar = 
  	(* Parse a simple, line-delimited, BNF Grammar. *)

  	let read line =
  		match bounded_split (regexp "[ \t]*:=[ \t]*") line 2 with
  			| [nonterminal; sequents] ->
  				let sequent_list = split (regexp "[ \t]+") sequents in
  				Some (nonterminal, sequent_list)
  			| _ -> None in

  	let read filename =
	  	let productions = ref []
	  	and file = open_in filename in
			try
			  printf "Parsing...\n";
			  while true; do
			  	let line = input_line file in
			  	match read line with
			  		| None ->
			  			printf "failed to match \"%s\"\n" line;
			  		| Some production ->
			  			productions := production :: !productions
			  done; []
			with End_of_file ->
				printf "Finished parsing.\n";
				close_in file;
			  rev !productions in

		let parsed = read filename in

		let nonterminals =
			let rec nonterminals = function
				| [] -> []
				| (nt,_)::tl -> nt::(nonterminals tl) in
			nonterminals parsed in

		let rec tag_sequents (productions: (string * string list) list): production list = 
			let map_production (nt, sequents) =
				(nt, map (fun sequent ->
					if mem sequent nonterminals then
						NT sequent
					else
						T sequent) sequents) in
			
			map map_production productions in

		tag_sequents parsed


	let string_of_production ((nt, sequents): production): string =
		(* Show a single production *)
		let rec show_sequents = function
			| [] -> ""
			| (NT nt)::tl -> sprintf " %s%s" nt (show_sequents tl)
			| (T t)::tl -> sprintf " %s%s" t (show_sequents tl) in
		sprintf "%s := %s" nt (show_sequents sequents)


	let show grammar: string =
		let rec show_sequents = function
			| [] -> ""
			| (NT nt)::tl -> sprintf " %s%s" nt (show_sequents tl)
			| (T t)::tl -> sprintf " %s%s" t (show_sequents tl) in
		String.concat "\n" (map (fun (nt, sequents) -> 
			sprintf "%s :=%s" nt (show_sequents sequents)) grammar)


	let write filename grammar =
		let file = open_out filename in
		iter (fun p -> output_string file ((string_of_production p) ^ "\n")) grammar;
		close_out file


	let sequents grammar: (terminal list) * (nonterminal list) =
		let accumulate_sequents (ts, nts) = function
			| T t -> (t::ts, nts)
			| NT nt -> (ts, nt::nts) in
		fold_left (fun (ts, nts) (nt, sequents) ->
			let nts = if not (mem nt nts) then nt::nts else nts in
			fold_left accumulate_sequents (ts, nts) sequents) ([], []) grammar
			

	let terminals grammar: terminal list = fst (sequents grammar)

	let nonterminals grammar: nonterminal list = snd (sequents grammar)



end