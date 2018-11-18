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
	cd src && dotnet fable npm-build

.PHONY: build-tests
build-tests: package-lock.json
	cd test && dotnet fable npm-build-test

.PHONY: test
test: build-tests
	npm test

.PHONY: test-watch-build
test-watch-build: package-lock.json
	cd test && dotnet fable npm-watch-test

.PHONY: test-watch-run
test-watch-run: package-lock.json
	./node_modules/.bin/jest --watch

.PHONY: distclean
distclean:
	rm -rf build

# -- BUILD RULES ------------------------------------------------------
package-lock.json: package.json
	npm install