build:
	stack build --fast

build-watch:
	stack build --fast --file-watch

clean:
	stack clean

tests:
	stack test --fast

tests-watch:
	stack test --fast --file-watch
