
OCB_FLAGS = -use-ocamlfind -pkg core -tags thread 
OCB =				ocamlbuild $(OCB_FLAGS)

MODULES = src/commands\
					src/watch\
					src/parser\
					src/typing\
					src/errors

INCLUDE_MODULES = $(foreach dir, $(MODULES), -I $(dir))

all: infrared_native

infrared_native:
	$(OCB) $(INCLUDE_MODULES) src/infrared.native

clean:
	$(OCB) -clean

.PHONY: clean all 

