module type CANVAS = sig
  type context

  type text_extents = {
    x_bearing : float;
    y_bearing : float;
    width : float;
    height : float;
    x_advance : float;
    y_advance : float;
  }

  type rectangle = {
    x : float;
    y : float;
    w : float;
    h : float;
  }

  val set_font_size : context -> float -> unit
  val set_line_width : context -> float -> unit
  val set_source_rgb : context -> r:float -> g:float -> b:float -> unit
  val set_source_rgba : context -> r:float -> g:float -> b:float -> a:float -> unit
  (* (Cairo needs to know the r,g,b too) *)
  val set_source_alpha : context -> r:float -> g:float -> b:float -> float -> unit
  val move_to : context -> x:float -> y:float -> unit
  val line_to : context -> x:float -> y:float -> unit
  val rectangle : context -> x:float -> y:float -> w:float -> h:float -> unit
  val stroke : context -> unit
  val stroke_preserve : context -> unit
  val fill : context -> unit
  val fill_preserve : context -> unit
  val text_extents : context -> string -> text_extents
  val paint_text : context -> ?clip_area:(float * float) -> x:float -> y:float -> string -> unit
  val paint : ?alpha:float -> context -> unit
  val clip_extents : context -> rectangle
end

let line_spacing = 32.
let big_text = 12.
let small_text = 8.

module Make (C : CANVAS) = struct
  (** Draw [msg] in the area (min_x, max_x) and ideally centred at [x]. *)
  let draw_label (v : View.t) cr ~min_x ~max_x ~x ~y msg =
    let text_width = C.((text_extents cr msg).x_advance) in
    let x =
      x -. (text_width /. 2.)   (* Desired start for centred text *)
      |> min (max_x -. text_width)
      |> max min_x in

    if x +. text_width > max_x then (
      (* Doesn't fit. Draw as much as we can. *)
      C.paint_text cr ~x:min_x ~y ~clip_area:(max_x -. x, v.height) msg
    ) else (
      (* Show label on left margin if the thread starts off-screen *)
      let x =
        if x < 4.0 then min 4.0 (max_x -. text_width)
        else x in
      C.paint_text cr ~x ~y msg
    )

  let bracket_width = 4.

  let draw_l_bracket cr ~x ~y ~w ~h =
    let w = min bracket_width w in
    C.move_to cr ~x:(x +. w) ~y;
    C.line_to cr ~x ~y;
    C.line_to cr ~x ~y:(y +. h);
    C.line_to cr ~x:(x +. w) ~y:(y +. h);
    C.stroke cr

  let draw_r_bracket cr ~x ~y ~w ~h =
    let w = min bracket_width w in
    C.move_to cr ~x:(x -. w) ~y;
    C.line_to cr ~x ~y;
    C.line_to cr ~x ~y:(y +. h);
    C.line_to cr ~x:(x -. w) ~y:(y +. h);
    C.stroke cr

  let rec render_events v cr (item : Model.item) =
    for i = 0 to Array.length item.events - 1 do
      let (ts, e) = item.events.(i) in
      let next =
        if i < Array.length item.events - 1 then
          Some (fst (item.events.(i + 1)))
        else item.end_time
      in
      match (e : Model.event) with
      | Add_fiber f -> render_fiber v cr ts f
      | Create_cc (ty, cc) -> render_cc v cr ts cc ty
      | Log msg ->
        let x = View.x_of_time v ts in
        let y = float item.y *. line_spacing +. 10. in
        C.set_source_rgb cr ~r:1.0 ~g:1.0 ~b:0.0;
        C.move_to cr ~x ~y;
        C.line_to cr ~x:(x -. 5.0) ~y:(y -. 5.0);
        C.line_to cr ~x:(x +. 5.0) ~y:(y -. 5.0);
        C.line_to cr ~x ~y;
        C.fill_preserve cr;
        C.line_to cr ~x ~y:(y +. 14.);
        C.set_source_rgb cr ~r:0.0 ~g:0.0 ~b:0.0;
        C.stroke cr;
        C.set_font_size cr big_text;
        let clip_area = next |> Option.map (fun t2 ->
            let x2 = View.x_of_time v t2 in
            (x2 -. x -. 2.0, v.height)
          ) in
        C.paint_text cr ~x:(x +. 2.) ~y:(y +. 12.) msg
          ?clip_area
    done

  and render_fiber v cr start_time (f : Model.item) =
    let y = float f.y *. line_spacing in
    let draw_act (t0, t1) =
      let x = View.x_of_time v t0 in
      let w = View.x_of_time v t1 -. x in
      C.rectangle cr ~x ~y:(y +. 8.) ~w ~h:18.;
    in
    C.set_source_rgb cr ~r:1.0 ~g:1.0 ~b:0.0;
    Array.iter draw_act f.activations;
    C.fill cr;
    let x = View.x_of_time v start_time in
    let w =
      match f.end_time with
      | None -> v.width -. min x 0.
      | Some stop -> View.x_of_time v stop -. x
    in
    C.set_source_rgb cr ~r:0.7 ~g:0.7 ~b:1.0;
    C.rectangle cr ~x ~y:(y +. 10.) ~w ~h:14.0;
    C.fill cr;
    C.set_source_rgb cr ~r:0.0 ~g:0.0 ~b:0.0;
    (*
  let label = string_of_int f.id in
  C.set_font_size cr big_text;
    draw_label v cr ~min_x:x ~max_x:(x +. w) ~x ~y:(y +. 12.) label;
    *)
    render_events v cr f

  and render_cc v cr start_time (cc : Model.item) ty =
    render_events v cr cc;
    let label = Eio_runtime_events.cc_ty_to_string ty in
    let x = View.x_of_time v start_time in
    let y = float cc.y *. line_spacing in
    let w =
      match cc.end_time with
      | None -> v.width -. x
      | Some stop -> View.x_of_time v stop -. x
    in
    let h = float cc.height *. line_spacing -. 4. in
    draw_l_bracket cr ~x ~y ~w ~h; 
    draw_r_bracket cr ~x:(x +. w) ~y ~w ~h; 
    C.set_source_rgb cr ~r:0.0 ~g:0.0 ~b:0.0;
    C.set_font_size cr small_text;
    let clip_width =
      match cc.end_cc_label with
      | Some t ->
        Fmt.epr "cc %d next event is at %f (ends at %f)@." cc.id t (Option.get cc.end_time);
        View.x_of_time v t -. x
      | None -> w
    in
    C.paint_text cr ~x:(x +. 2.) ~y:(y +. 8.) ~clip_area:(clip_width -. 2., v.height) label

  let render_grid v cr =
    C.set_line_width cr 1.0;
    C.set_source_rgb cr ~r:0.7 ~g:0.7 ~b:0.7;
    let clip = C.clip_extents cr in
    let grid_step, grid_start_x, grid_step_x = View.grid v clip.x in
    let rec draw x =
      if x < clip.x +. clip.w then (
        C.move_to cr ~x:x ~y:clip.y;
        C.line_to cr ~x:x ~y:(clip.y +. clip.h);
        C.stroke cr;
        draw (x +. grid_step_x)
      ) in
    draw grid_start_x;
    C.set_source_rgb cr ~r:0.4 ~g:0.4 ~b:0.4;
    let msg =
      if grid_step >= 1.0 then Printf.sprintf "Each grid division: %.0f s" grid_step
      else if grid_step >= 0.001 then Printf.sprintf "Each grid division: %.0f ms" (grid_step *. 1000.)
      else if grid_step >= 0.000_001 then Printf.sprintf "Each grid division: %.0f us" (grid_step *. 1_000_000.)
      else if grid_step >= 0.000_000_001 then Printf.sprintf "Each grid division: %.0f ns" (grid_step *. 1_000_000_000.)
      else Printf.sprintf "Each grid division: %.2g s" grid_step in
    let extents = C.text_extents cr msg in
    let y = v.height -. C.(extents.height +. extents.y_bearing) -. 2.0 in
    C.paint_text cr ~x:4.0 ~y msg

  let render (v : View.t) cr =
    C.set_source_rgb cr ~r:0.9 ~g:0.9 ~b:0.9;
    C.paint cr;
    render_grid v cr;
    C.set_source_rgb cr ~r:0.0 ~g:0.0 ~b:0.0;
    render_events v cr v.model.root
end
