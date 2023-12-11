type t = {
  model : Model.t;
  mutable width : float;
  mutable height : float;
  mutable start_time : float;
  mutable pixels_per_ns : float;
}

let x_of_time t time = (time -. t.start_time) *. t.pixels_per_ns
let time_of_x t x = x /. t.pixels_per_ns +. t.start_time

let timespan_of_width t x = x /. t.pixels_per_ns

let grid t x =
  let grid_step = (* ns per grid step *)
    let l = 2.5 -. log10 t.pixels_per_ns |> floor in
    10. ** l
  in
  let grid_step_x = grid_step *. t.pixels_per_ns in
  let starting_grid_line = floor (x /. t.pixels_per_ns /. grid_step) in
  let grid_start_x = (starting_grid_line *. grid_step_x) -. t.start_time *. t.pixels_per_ns in
  grid_step *. 1e-9, grid_start_x, grid_step_x

let zoom t factor =
  t.pixels_per_ns <- t.pixels_per_ns *. factor

let set_start_time t time =
  t.start_time <- max 0.0 time

let of_model model ~width ~height =
  Model.layout model;
  { model; width; height; start_time = 0.; pixels_per_ns = 1e-3 }
