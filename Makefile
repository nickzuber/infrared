
OCB_FLAGS = -use-ocamlfind -pkg core,batteries,ppx_deriving.show -tags thread
OCB =       ocamlbuild $(OCB_FLAGS)

MODULES = src/commands \
          src/watch \
          src/parser \
          src/typechecker \
          src/errors \
          core \
          utils

INCLUDE_MODULES = $(foreach dir, $(MODULES), -I $(dir))

all: infrared

run:
	make && make test

infrared:
	$(OCB) $(INCLUDE_MODULES) src/infrared.native

version:
	./infrared.native version

test:
	python tests/run_tests.py

generate-tests:
	python tests/generate_tests.py

# @TEMP
e-test:
	./infrared.native parse tests/_experimental/test.js
e-test-s:
	./infrared.native parse tests/_experimental/test.js | python -m json.tool > tests/_experimental/test.exp
e-test-p:
	./infrared.native parse tests/_experimental/test.js | python -m json.tool

clean:
	$(OCB) -clean

.PHONY: all run infrared test generate_tests clean 

