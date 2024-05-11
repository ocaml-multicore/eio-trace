(* This runs jobs 4 at a time. It's useful to check eio-trace's layout algorithm. *)

open Eio.Std

let lock = Eio.Mutex.create ()

let main () =
  List.init 15 Fun.id
  |> Fiber.List.iter ~max_fibers:4 (fun i ->
      if i = 4 then Fiber.both Fiber.yield Fiber.yield;
      Eio.Mutex.use_ro lock Fiber.yield)

let () =
  Eio_mock.Backend.run main
