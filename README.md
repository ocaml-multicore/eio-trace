# eio-trace

Status: early prototype, and requires a development version of Eio

eio-trace can be used to record and display traces of programs using the [Eio][] library.

To install eio-trace:

```
git clone --recursive https://github.com/ocaml-multicore/eio-trace.git
cd eio-trace
dune build
```

To record a trace:

```
dune exec -- eio-trace record -f trace.fxt -- myprog ...
```

This runs `myprog ...` with the `OCAML_RUNTIME_EVENTS_START` environment variable set, which causes it to record events to a ring buffer.
eio-trace saves these events to `trace.fxt`.

You might like to start by tracing the example that comes with eio-trace:

```
dune exec -- eio-trace record -f trace.fxt -- ./_build/default/examples/net/main.exe
```

The trace can be viewed using generic tools such as [Perfetto][], but eio-trace's own visualisation is more useful,
as it takes advantage of Eio's structured concurrency:

```
dune exec -- eio-trace show -f trace.fxt
```

[Eio]: https://github.com/ocaml-multicore/eio
[Perfetto]: https://ui.perfetto.dev/
