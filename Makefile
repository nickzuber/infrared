
run-debug:
	@echo "\n\033[0;36m=-=-= \033[0;1mBuild & Run Debug \033[0;36m=-=-=-=-=-=-=-=-=-=-=\033[0m"
	make -C infrared-core/
	npm run debug

run-clean:
	@echo "\n\033[0;36m=-=-= \033[0;1mClean Infrared Core \033[0;36m=-=-=-=-=-=-=-=-=-=-=\033[0m"
	make clean -C infrared-core/

.PHONY: run-debug run-clean
