.PHONY: clean build

FLOW_VERSION = 0.70.0

clean:
	@rm -rf flow _build node_modules

flow:
	@git clone git@github.com:facebook/flow.git
	@cd flow && git checkout v$(FLOW_VERSION)

install:
	@esy install

build: install
	@esy build

test: build
	@esy x test_flow_parser.exe
