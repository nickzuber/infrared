COMPILER = -gcc
FLAGS    = 

BUILD += core

# Build everything
all: $(BUILD)

# Build exec structure for threading
core: core.h core.c
	$(COMPILER) $(FLAGS) -c exec/core.c
