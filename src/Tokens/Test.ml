open StreamParsing
open RegularExpressions
open String

type test = {
	regex: string;
	literal: string;
	matches: (int * int) list
}

let read_tests (filename: string): test list =
	
	let rec read_test line =

		let rec read_matches ?matches:(matches=[]) = function
			| "" | "NOMATCH" -> matches
			| str ->
				let comma = index str ','
				and rparen = index str ')' in
				let x = int_of_string (sub str 1 (comma - 1))
				and y = int_of_string (sub str (comma + 1) (rparen - comma - 1)) in
				read_matches ~matches:((x,y)::matches) (sub str (rparen + 1) ((length str) - rparen - 1)) in

		let first_tab = index line '\t' in
		let second_tab = index_from line (first_tab + 1) '\t' in
		{ regex = sub line 0 (first_tab);
			literal =	sub line first_tab (second_tab - first_tab);
			matches = read_matches (sub line (second_tab+1) ((length line) - second_tab - 1)) } in

	let input = open_in filename
	and tests = ref [] in

	(try
		while true do
			let test = read_test (input_line input) in
			tests := test::!tests
		done
	with End_of_file ->
		close_in input);

	List.rev !tests

let rec run_tests ?line_no:(line_no=1) = function
	| [] -> ()
	| test::tests ->
		print_string ((string_of_int line_no)^"\t"^test.regex^"\t");
		let regex = (
			try
				Enhanced.read (Stream.of_string test.regex)
			with
				StreamParsingExc m ->
					print_endline ("PARSE_FAILURE\t"^m);
					failwith m) in
		let regex_string = Enhanced.show regex in
		if test.regex = regex_string then
			Printf.printf "%s\tPASS\n" (Enhanced.show regex)
		else
			Printf.printf "%s\tFAIL\n" (Enhanced.show regex);
		run_tests ~line_no:(line_no+1) tests






let _ =
	let tests = read_tests "./Test.data" in
	run_tests tests
	(*
	let channel = open_in "./Test.data" in
	let token_map = TokenMap.from_channel channel in
	Hashtbl.iter (fun label r ->
		printf "(%s" label;
		open_box 2;
		force_newline();
		Enhanced.inspect r;
		print_string ")";
		close_box();
		force_newline();) token_map;*)

	(*print_string (TokenMap.show token_map)*)