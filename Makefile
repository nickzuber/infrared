OCB_FLAGS = -use-ocamlfind -pkg core,batteries,ppx_deriving.show -tags thread
OCB =       ocamlbuild $(OCB_FLAGS)

MODULES = src/commands \
          src/watch \
          src/parser \
          src/parser/lexing_components \
          src/parser/parsing_components \
          src/parser/ast_components \
          src/typechecker \
          src/errors \
          core \
          utils

INCLUDE_MODULES = $(foreach dir, $(MODULES), -I $(dir))

all: build

run:
	make && make test

build:
	$(OCB) $(INCLUDE_MODULES) src/infrared.native

version:
	./infrared.native version

test:
	python tests/run_tests.py

generate-tests:
	python tests/generate_tests.py

try:
	./infrared.native parse tests/_experimental/test.js

try-save:
	./infrared.native parse tests/_experimental/test.js | python -m json.tool > tests/_experimental/test.json
	./infrared.native parse tests/_experimental/test.js | python -m json.tool > viewer/public/test.json

try-pretty:
	./infrared.native parse tests/_experimental/test.js | python -m json.tool

view:
	./infrared.native parse tests/_experimental/test.js | python -m json.tool > viewer/public/test.json
	node viewer/ast_to_treedata.js > viewer/public/treeData.json && \
	node viewer/app.js

clean:
	$(OCB) -clean

.PHONY: all run build version test generate-tests try try-save try-pretty view clean
