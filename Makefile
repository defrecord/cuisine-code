# Cuisine Code Makefile
# Copyright (c) 2025 Aidan Pace

.PHONY: all clean setup deps compile-scheme compile-c compile-wasm serve test doc

# Directories
SCHEME_SRC = ./scheme/src
SCHEME_TESTS = ./scheme/tests
C_OUTPUT = ./c-output/src
WASM_OUTPUT = ./web/wasm
WEB_SRC = ./web/src
DOCS = ./docs

# Tools
GUILE = guile
EMCC = emcc
NODE = node
MERMAID = mmdc

# Compilation flags
EMCC_FLAGS = -s WASM=1 -s EXPORTED_RUNTIME_METHODS=['ccall','cwrap'] -s EXPORTED_FUNCTIONS=['_run_kitchen'] -s ALLOW_MEMORY_GROWTH=1

all: setup compile-scheme compile-c compile-wasm doc

# Setup project structure
setup:
	@echo "Setting up project structure..."
	@bash scripts/create-dirs.sh

# Install dependencies
deps:
	@echo "Installing dependencies..."
	@bash scripts/deps.sh

# Compile Scheme to C
compile-scheme:
	@echo "Compiling Scheme to C..."
	mkdir -p $(C_OUTPUT)
	$(GUILE) ./tools/scheme-to-c.scm $(SCHEME_SRC)/core/kitchen/kitchen.scm > $(C_OUTPUT)/cuisine_code.c

# Compile C to object files
compile-c:
	@echo "Compiling C..."
	mkdir -p $(C_OUTPUT)/build
	cc -c $(C_OUTPUT)/cuisine_code.c -o $(C_OUTPUT)/build/cuisine_code.o

# Compile to WebAssembly
compile-wasm:
	@echo "Compiling to WebAssembly..."
	mkdir -p $(WASM_OUTPUT)
	$(EMCC) $(EMCC_FLAGS) $(C_OUTPUT)/cuisine_code.c -o $(WASM_OUTPUT)/cuisine_code.js

# Start development server
serve:
	@echo "Starting development server..."
	$(NODE) ./tools/dev-server.js

# Run tests
test:
	@echo "Running tests..."
	$(GUILE) -L . -e "(execute-test-suite)" ./scripts/run-tests.scm

# Generate documentation
doc:
	@echo "Generating documentation..."
	@mkdir -p $(DOCS)/diagrams
	@for f in $(DOCS)/*.mmd; do \
		if [ -f "$$f" ]; then \
			$(MERMAID) -i "$$f" -o "$(DOCS)/diagrams/$$(basename "$$f" .mmd).png"; \
		fi \
	done

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	rm -rf $(C_OUTPUT)/build
	rm -f $(C_OUTPUT)/cuisine_code.c
	rm -f $(WASM_OUTPUT)/cuisine_code.js
	rm -f $(WASM_OUTPUT)/cuisine_code.wasm
