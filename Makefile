all:
	dune build @all @runtest

example:
	dune build ./examples/net/main.exe @install
	dune exec -- eio-trace run -- ./_build/default/examples/net/main.exe
