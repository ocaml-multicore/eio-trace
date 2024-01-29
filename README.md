# eio-trace

eio-trace can be used to record and display traces of programs using the [Eio][] library
(requires Eio 0.14 or later).

To install eio-trace:

```
git clone --recursive https://github.com/ocaml-multicore/eio-trace.git
cd eio-trace
dune build
```

To run an Eio program and display the trace:

```
dune exec -- eio-trace run -- myprog ...
```

You might like to start by tracing the example that comes with eio-trace:

```
dune exec -- eio-trace run -- ./_build/default/examples/net/main.exe
```

<p align='center'>
  <img src="./doc/net.svg"/>
</p>

To record a trace:

```
dune exec -- eio-trace record -f trace.fxt -- myprog ...
```

This runs `myprog ...` with the `OCAML_RUNTIME_EVENTS_START` environment variable set, which causes it to record events to a ring buffer.
eio-trace saves these events to `trace.fxt`.


The trace can be viewed using generic tools such as [Perfetto][], but eio-trace's own visualisation is more useful,
as it takes advantage of Eio's structured concurrency:

```
dune exec -- eio-trace show trace.fxt
```

To convert a trace to SVG format:

```
dune exec -- eio-trace render -f trace.fxt trace.svg
```

[Eio]: https://github.com/ocaml-multicore/eio
[Perfetto]: https://ui.perfetto.dev/

## macOS

These instructions will walk you through installing the system dependencies necessary in order to install eio-trace's dependencies from OPAM.

These instructions make use of [Homebrew](https://brew.sh/), adapt them to MacPorts, Fink, etc., if needed.

First ensure everything is up-to-date:
```sh
brew update
brew upgrade
opam update
```

Then install the required packages:
```sh
brew install bash cairo gtk+3 m4 pkg-config libffi
```

Consider restarting your terminal session at this stage.

You can now install eio-trace's dependencies.

### lablgtk3

If installing `conf-gtk3` fails with a `libffi` version error, run the following before trying again:
```sh
export PKG_CONFIG_PATH=/usr/local/Cellar/libffi/3.4.4/lib/pkgconfig
```
Update the version number to match the currently installed version of libffi.
