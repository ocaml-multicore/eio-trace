all:
	dune build @all @runtest

example:
	dune build ./examples/net/main.exe
	dune exec -- eio-trace record -f trace.fxt -- ./_build/default/examples/net/main.exe
	dune exec -- eio-trace-gtk trace.fxt
