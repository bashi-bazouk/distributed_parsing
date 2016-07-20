open CFG

let () = 
	let sg = (StringGrammar.read "test/input_grammar.bnf") in
	print_endline (StringGrammar.show sg)
	