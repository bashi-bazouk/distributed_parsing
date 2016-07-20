.PHONY: test clean

build/CFG.cmo: src/CFG.ml
	ocamlc -c str.cma src/CFG.ml
	mv src/CFG.cm* build

main: test/main.ml build/CFG.cmo
	ocamlc -o main -I ./build str.cma CFG.cmo test/main.ml

test: main
	./main

clean:
	-find . -regex '.*\.cm.' -exec rm {} \;
	-find . -regex '.*~' -exec rm {} \;
	-rm main;