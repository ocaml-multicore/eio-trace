all:
	dune build @all @runtest

example:
	dune build ./examples/net/main.exe @install
	dune exec -- eio-trace record -- ./_build/default/examples/net/main.exe
	dune exec -- eio-trace show
