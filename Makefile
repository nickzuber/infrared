
OCB_FLAGS = -use-ocamlfind -pkg core,batteries -tags thread 
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

# Run test on testing file and compare to actual expected output file
# We generate those expected output files in a controlled env
test:
	echo "not implemented yet"

clean:
	$(OCB) -clean

.PHONY: clean all run

