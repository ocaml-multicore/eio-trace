all:
	dune build @all @runtest

example:
	dune build ./examples/net/main.exe
	dune exec eio-trace -- ./_build/default/examples/net/main.exe
