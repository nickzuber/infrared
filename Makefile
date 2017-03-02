
OCB_FLAGS = -use-ocamlfind -pkg core -tags thread 
OCB =				ocamlbuild $(OCB_FLAGS)

MODULES = src/commands\
					src/watch\
					src/parser\
					src/typechecker\
					src/errors\
					core\
					utils

INCLUDE_MODULES = $(foreach dir, $(MODULES), -I $(dir))

all: infrared

run:
	make && ./infrared.native

infrared:
	$(OCB) $(INCLUDE_MODULES) src/infrared.native

test:
	echo "no tests yet"

clean:
	$(OCB) -clean

.PHONY: clean all run

