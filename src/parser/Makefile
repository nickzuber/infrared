
warn:
	echo "\tocamlbuild knows how to handle ocamllex by itself"
	echo "\tso don't use this makefile as a build step"

all:
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlfind ocamlc -c -linkpkg -package batteries parser.ml
	ocamlfind ocamlc -o obj -linkpkg\
		-package batteries lexer.cmo parser.cmo

clean:
	rm *.cmo
	rm *.cmi
	rm lexer.ml
	rm obj

restart:
	make clean && make all

.PHONY: all clean restart

