

let rec fixpoint ?eq:(eq=(=)) f init =
	let prev = ref init
	and next = ref (f init) in
	while not (eq !prev !next) do
		prev := !next;
		next := f !next
	done;
	!next