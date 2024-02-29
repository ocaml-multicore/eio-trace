type t = {
  mutable layout : Layout.t;
  mutable width : float;                (* Canvas width in pixels *)
  mutable height : float;
  mutable start_time : float;           (* Time after layout start (ns) *)
  mutable scroll_y : float;             (* Pixels *)
  mutable pixels_per_ns : float;
  mutable zoom : float;
}

let h_margin = 4.
let v_margin = 4.

let pixels_per_row = 32.

let clamp ~min:a ~max:b (v : float) =
  max a (min b v)

let x_of_time t time =
  let x = (time -. t.start_time) *. t.pixels_per_ns in
  clamp x ~min:(-. 100.) ~max:(t.width +. 100.)

let time_of_x t x = x /. t.pixels_per_ns +. t.start_time

let width_of_timespan t ts = ts *. t.pixels_per_ns
let timespan_of_width t x = x /. t.pixels_per_ns

let grid t x =
  let grid_step = (* ns per grid step *)
    let l = 2.5 -. log10 t.pixels_per_ns |> floor in
    10. ** l
  in
  let starting_grid_line = floor (time_of_x t x /. grid_step) in
  let grid_step_x = grid_step *. t.pixels_per_ns in (* pixels per grid step *)
  let grid_start_x = (starting_grid_line *. grid_step_x) -. t.start_time *. t.pixels_per_ns in
  grid_step *. 1e-9, grid_start_x, grid_step_x

let zoom_to t z =
  t.zoom <- clamp z ~min:(-.15.) ~max:2.5;
  t.pixels_per_ns <- 10. ** t.zoom

let zoom t delta =
  zoom_to t (t.zoom +. delta)

let zoom_to_fit ?(start_time=0.0) ?duration t =
  let start_time = min start_time t.layout.duration in
  let duration = Option.value duration ~default:(t.layout.duration -. start_time) in
  let ppns = (t.width -. 2. *. h_margin) /. duration in
  zoom_to t (log ppns /. log 10.);
  t.start_time <- start_time -. timespan_of_width t h_margin

let max_x_scroll t =
  width_of_timespan t t.layout.duration +. h_margin

let max_y_scroll t =
  float t.layout.height *. pixels_per_row +. v_margin

let scroll_bounds t =
  (
    (-. h_margin, max_x_scroll t +. t.width, t.width, t.start_time *. t.pixels_per_ns),
    (-. v_margin, max_y_scroll t +. t.height, t.height, t.scroll_y)
  )

let set_start_time t time =
  let margin_time = timespan_of_width t h_margin in
  t.start_time <- clamp time ~min:(-. margin_time) ~max:(t.layout.duration +. margin_time);
  t.start_time *. t.pixels_per_ns

let set_scroll_y t y =
  t.scroll_y <- clamp y ~min:(-. v_margin) ~max:(max_y_scroll t);
  y

let set_size t width height =
  t.width <- width;
  t.height <- height

let of_layout layout ~width ~height =
  let t = { layout; width; height; start_time = 0.; scroll_y = -. v_margin; pixels_per_ns = 0.0; zoom = -3.0 } in
  zoom t 0.0;
  t

let set_layout t layout =
  t.layout <- layout
