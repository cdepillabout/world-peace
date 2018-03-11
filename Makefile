.PHONY: build build-example clean dump-splices dump-th example-client example-docs example-server ghci haddock haddock-server lint test upload watch watch-example watch-haddock watch-test
all: build

build:
	stack build

clean:
	stack clean

# dump the template haskell
dump-splices: dump-th
dump-th:
	mkdir -p test/test-dir/empty-dir
	-stack build --ghc-options="-ddump-splices"
	@echo
	@echo "Splice files:"
	@echo
	@find "$$(stack path --dist-dir)" -name "*.dump-splices" | sort

haddock:
	stack build --haddock

# Run ghci using stack.
ghci:
	stack ghci

test:
	stack test

# Run hlint.
lint:
	hlint src/

# This runs a small python websever on port 8001 serving up haddocks for
# packages you have installed.
#
# In order to run this, you need to have run `make build-haddock`.
haddock-server:
	cd "$$(stack path --local-doc-root)" && python -m http.server 8001

# Upload this package to hackage.
upload:
	stack upload .

# Watch for changes.
watch:
	stack build --file-watch --fast .

watch-haddock:
	stack build --haddock --file-watch --fast .

watch-test:
	stack test --file-watch --fast .

