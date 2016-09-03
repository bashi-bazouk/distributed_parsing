open Printf
open Stream

let rec njunk (input: 'a Stream.t): int -> unit = function 
	| 0 -> ()
	| n -> Stream.junk input; njunk input (n-1)


let npeek_string (n: int) (input: char Stream.t): string =
	let seen = npeek n input in
	String.init (List.length seen) (fun i -> List.nth seen i)


(* Erroring out *)

exception StreamParsingExc of string

let fail ?error input = 
	match error with
		| Some message -> 
			raise (StreamParsingExc (Printf.sprintf "Syntax error at %i: %s" (count input) message))
		| None ->
			raise (StreamParsingExc (Printf.sprintf "Syntax error at %i." (count input)))


let assert_one (input: char Stream.t) (error: string) =
	match peek input with
		| Some c -> c
		| None -> fail ~error:error input


let rec assert_n (n: int) (input: char Stream.t) (error: string) =
	let result = npeek n input in
	if (List.length result) = n then
		let s = String.init n (fun i -> List.nth result i) in
		s
	else
		fail ~error:error input


let assert_prefix (p: string) (input: char Stream.t) (error: string) =
	if p <> (assert_n (String.length p) input error) then
		fail ~error:error input

let guard (input: char Stream.t) (t: char Stream.t -> unit) (e: char Stream.t -> 'a): 'a option =
	let guard_checked = (
		try (
			t input;
			true
		) with 
			StreamParsingExc _ -> false) in
	if guard_checked then
		Some (e input)
	else
		None


let rec guard_by_prefix (input: char Stream.t) (guards: (string * (char Stream.t -> 'a)) list): 'a option = 
	(* `guard_by_prefix rules` executes the callback of the first guard string to match the input *)
	match guards with 
		| [] -> None
		| (guard_prefix, callback)::tl -> (
			match guard input (fun input -> assert_prefix guard_prefix input "guard_by_prefix") callback with
				| Some x -> Some x
				| None -> guard_by_prefix input tl
			)


let rec take_while ?prefix:(prefix="") (input: char Stream.t) (p: char -> bool): string =
	match peek input with
		| None -> prefix
		| Some c ->
			if p c then (
				junk input;
				take_while ~prefix:(prefix^(String.make 1 c)) input p)
			else
				prefix


let drop_whitespace input =
	ignore (take_while input (fun c -> (c = ' ') || (c = '\t')))


let take_decimal input =
	let is_decimal c =
		let ascii = int_of_char c in
		(48 <= ascii) && (ascii <= 57) in
	int_of_string (take_while input is_decimal)


let rec dump ?count:(count=100) input =
	if count >= 0 then
		match peek input with
			| None -> ()
			| Some _ ->
				print_char (next input);
				print_char '\n';
				dump ~count:(count-1) input

(*let junk input = print_char (assert_one input "char?"); print_char '\n'; junk input*)