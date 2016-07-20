.PHONY: test

build/CFG.cmo: src/CFG.ml
	ocamlc -a str.cma src/CFG.ml -o build/CFG.cma
	mv src/CFG.cmi build/CFG.cmi

main: test/main.ml build/CFG.cmo
	ocamlc -o main -I ./build CFG.cma test/main.ml

test: main
	./main