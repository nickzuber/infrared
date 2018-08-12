
run-debug:
	@echo "\n\033[0;36m=-=-= \033[0;1mBuild & Run Debug \033[0;36m=-=-=-=-=-=-=-=-=-=-=\033[0m"
	make -C infrared-core/
	npm run debug

.PHONY: run-debug
