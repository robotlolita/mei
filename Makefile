BIN := ./node_modules/.bin
SOURCES := $(wildcard src/*.fs) $(wildcard src/*/*.fs)
FABLE_SPLITTER := $(BIN)/fable-splitter
JEST := $(BIN)/jest

# -- TASKS ------------------------------------------------------------
.PHONY: help
help:
	@echo "Building:"
	@echo "---------"
	@echo "  build              Compiles the project"
	@echo "  build-tests        Compiles the tests"
	@echo ""
	@echo "Testing:"
	@echo "--------"
	@echo "  test               Compiles and runs the tests"
	@echo "  test-watch-build   Continuously compiles the tests"
	@echo "  test-watch-run     Continuously runs the tests"
	@echo ""
	@echo "Cleaning up:"
	@echo "------------"
	@echo "  distclean          Cleans up built artifacts"
	@echo "  clean              Cleans up the source folders"


.PHONY: build
build:
	$(FABLE_SPLITTER) --config Mei/src/splitter.config.js

.PHONY: build-tests
build-tests:
	$(FABLE_SPLITTER) --config Mei/test/splitter.config.js

.PHONY: test
test: build-tests
	$(JEST)

.PHONY: test-watch-build
test-watch-build:
	$(FABLE_SPLITTER) --config Mei/test/splitter.config.js --watch

.PHONY: test-watch-run
test-watch-run:
	$(JEST) --watch

.PHONY: distclean
distclean:
	rm -rf build


# -- EXAMPLES ---------------------------------------------------------
ALL_EXAMPLES := examples/greeting/build/hello.js examples/naval-fate/build/NavalFate.js

examples: $(ALL_EXAMPLES)

examples/greeting/build/hello.js: examples/greeting/hello.fsproj examples/greeting/hello.fs
	$(FABLE_SPLITTER) --config examples/greeting/splitter.config.js

examples/naval-fate/build/NavalFate.js: examples/naval-fate/NavalFate.fsproj examples/naval-fate/NavalFate.fs
	$(FABLE_SPLITTER) --config examples/naval-fate/splitter.config.js
