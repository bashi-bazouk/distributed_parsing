module Regex = RegularExpressions.Enhanced

(* Parser in the Hadoop Streaming format. *)

type token_map = (string, Regex.t) Hashtbl.t

let from_channel (channel: in_channel): token_map =
	let token_map = Hashtbl.create 16 in

	let line_count = ref 0
	and not_done = ref true in
	while !not_done do
		incr line_count;
		try
			let line = input_line channel in
			if line = "" then
				() 
			else
				let split_on =
					(try
						String.index line '\t'
					with Not_found ->
						failwith (Printf.sprintf "Invalid token at %i: missing tab delimiter." !line_count)) in
					
				let label = String.sub line 0 split_on in

				let regex = (
					try 
						Regex.read (Stream.of_string (String.sub line (split_on+1) ((String.length line) - split_on - 1)))
					with
						StreamParsing.StreamParsingExc msg ->
							failwith (Printf.sprintf "Syntax error in regular expression at line %i:\n  %s" !line_count msg)
					) in

				if Hashtbl.mem token_map label then
					Hashtbl.replace token_map label (Regex.Disjunction((Hashtbl.find token_map label), regex))
				else
					Hashtbl.add token_map label regex

		with End_of_file ->
			not_done := false
	done;
	token_map


let show (t: token_map): string =
	let buf = Buffer.create 16 in

	let rec add_rule label = function
		| Regex.Disjunction(l,r) ->
			add_rule label l;
			add_rule label r
		| regex ->
			Buffer.add_string buf label;
			Buffer.add_char buf '\t';
			Buffer.add_string buf (Regex.show regex);
			Buffer.add_char buf '\n' in

	let labels = (Hashtbl.fold (fun k _ acc -> k::acc) t []) in

	List.iter 
		(fun label -> add_rule label (Hashtbl.find t label))
		(List.sort compare labels);

	Buffer.contents buf