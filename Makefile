pprintf = @printf "\n\033[36m<><>\033[39m \033[1m%s\033[0m \033[36m<><><><><><><><><><><><><><><><><><><><><><>\033[39m 🚀\n" "$(1)"
p_update = @printf "\033[33m ↻\033[39m \033[89m%s\033[39m\n" "$(1)"
p_add = @printf "\033[32m ↗\033[39m \033[89m%s\033[39m\n" "$(1)"
p_remove = @printf "\033[31m ↘\033[39m \033[89m%s\033[39m\n" "$(1)"
p_dot = @printf "\033[32m ∗\033[39m \033[89m%s\033[39m\n" "$(1)"
p_dot_red = @printf "\033[31m ∗\033[39m \033[89m%s\033[39m\n" "$(1)"

.PHONY: build test test-coverage clean

build:
	$(call pprintf,Building project)
	dune build Infrared/main.exe
	cp _build/default/Infrared/main.exe bin/infrared.exe
	$(call p_dot,Done.)

test:
	$(call pprintf,Running tests for project)
	make test-coverage
	$(call p_dot,Done.)

test-coverage:
	$(call pprintf,Testing code coverage)
	sh test-coverage/Parser/run.sh

test-coverage-verbose:
	$(call pprintf,Testing code coverage)
	sh test-coverage/Parser/run.sh --verbose

clean:
	$(call pprintf,Cleaning project)
	dune clean
	$(call p_dot,Done.)
