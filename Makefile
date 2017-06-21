
OCB_FLAGS = -use-ocamlfind -pkg core,batteries -tags thread 
OCB =       ocamlbuild $(OCB_FLAGS)

MODULES = src/commands \
          src/watch \
          src/parser \
          src/typechecker \
          src/errors \
          core \
          utils

MODULES_FOR_TESTING = src/parser \
					  tests/ocaml

INCLUDE_MODULES = $(foreach dir, $(MODULES), -I $(dir))
INCLUDE_MODULES_FOR_TESTING = $(foreach dir, $(MODULES_FOR_TESTING), -I $(dir))

all: infrared

run:
	make && make test

infrared:
	$(OCB) $(INCLUDE_MODULES) src/infrared.native

test:
	python tests/run_tests.py

ocaml-test:
	$(OCB) $(INCLUDE_MODULES_FOR_TESTING) tests/ocaml/main.native

generate-tests:
	python tests/generate_tests.py

# @TEMP
e-test:
	./infrared.native parse tests/_experimental/test.js
e-test-s:
	./infrared.native parse tests/_experimental/test.js > tests/_experimental/test.exp

clean:
	$(OCB) -clean

.PHONY: all run infrared test generate_tests clean 

