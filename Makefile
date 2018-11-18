SOURCES := $(wildcard src/*.fs) $(wildcard src/*/*.fs)

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
build: package-lock.json
	cd Mei/src && dotnet fable fable-splitter -- --config Mei/src/splitter.config.js

.PHONY: build-tests
build-tests: package-lock.json
	cd Mei/test && dotnet fable fable-splitter -- --config Mei/test/splitter.config.js

.PHONY: test
test: build-tests
	./node_modules/.bin/jest

.PHONY: test-watch-build
test-watch-build: package-lock.json
	cd Mei/test && dotnet fable fable-splitter -- --config Mei/test/splitter.config.js --watch

.PHONY: test-watch-run
test-watch-run: package-lock.json
	./node_modules/.bin/jest --watch

.PHONY: distclean
distclean:
	rm -rf build

# -- BUILD RULES ------------------------------------------------------
package-lock.json: package.json
	npm install


# -- EXAMPLES ---------------------------------------------------------
ALL_EXAMPLES := examples/greeting/build/hello.js examples/naval-fate/build/NavalFate.js

examples: $(ALL_EXAMPLES)

examples/greeting/build/hello.js: examples/greeting/hello.fsproj examples/greeting/hello.fs
	cd examples/greeting && dotnet fable fable-splitter -- --config examples/greeting/splitter.config.js

examples/naval-fate/build/NavalFate.js: examples/naval-fate/NavalFate.fsproj examples/naval-fate/NavalFate.fs
	cd examples/naval-fate && dotnet fable fable-splitter -- --config examples/naval-fate/splitter.config.js
