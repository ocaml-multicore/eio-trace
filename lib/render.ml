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

module Make (C : CANVAS) = struct
  module Style = struct
    let line_spacing = View.pixels_per_row
    let big_text = 12.
    let small_text = 8.

    let fiber_padding_top = 10.0
    let fiber_height = 14.0

    let running_fiber cr =
      C.set_source_rgb cr ~r:0.4 ~g:0.8 ~b:0.4
  end

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

  let y_of_row v row =
    float row *. Style.line_spacing -. v.View.scroll_y

  let iter_spans v fn item =
    Array.iter fn item.Model.activations;
    let stop = Option.value item.end_time ~default:v.View.model.duration in
    fn (stop, [])

  let link_fibers v cr ~x a b =
    let upper, lower = if a.Model.y < b.Model.y then a, b else b, a in
    C.move_to cr ~x ~y:(y_of_row v upper.y +. Style.fiber_padding_top);
    C.line_to cr ~x ~y:(y_of_row v lower.y +. Style.fiber_padding_top +. Style.fiber_height);
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
      | Add_fiber { parent; child } ->
        let parent = Model.get v.View.model parent |> Option.value ~default:item in
        render_fiber v cr ts child;
        Style.running_fiber cr;
        let x = View.x_of_time v ts in
        link_fibers v cr ~x parent child
      | Create_cc (ty, cc) -> render_cc v cr ts cc ty
      | Log msg ->
        let x = View.x_of_time v ts in
        let y = y_of_row v item.y +. 10. in
        C.move_to cr ~x ~y:(y +. 3.);
        C.line_to cr ~x ~y:(y -. 3.);
        C.set_source_rgb cr ~r:0.0 ~g:0.0 ~b:0.0;
        C.stroke cr;
        C.set_font_size cr Style.small_text;
        let clip_area = next |> Option.map (fun t2 ->
            let x2 = View.x_of_time v t2 in
            (x2 -. x -. 2.0, v.height)
          ) in
        C.paint_text cr ~x:(x +. 2.) ~y:(y -. 2.) msg
          ?clip_area
    done

  and render_fiber v cr start_time (f : Model.item) =
    let y = y_of_row v f.y in
    if y < v.height then (
(*
    let x = View.x_of_time v start_time in
    let w =
      match f.end_time with
      | None -> v.width -. min x 0.
      | Some stop -> View.x_of_time v stop -. x
    in
*)
      if y +. View.pixels_per_row > 0. then (
        C.set_font_size cr Style.big_text;
        let prev_stack = ref [] in
        let event = ref (start_time, []) in
        f |> iter_spans v (fun event' ->
            let t0, stack = !event in
            event := event';
            let t1 = fst event' in
            let x0 = View.x_of_time v t0 in
            let x1 = View.x_of_time v t1 in
            let w = x1 -. x0 in
            begin match stack with
              | `Suspend _ :: _ -> C.set_source_rgb cr ~r:0.4 ~g:0.4 ~b:0.4
              | `Span _ :: _ -> C.set_source_rgb cr ~r:0.5 ~g:0.9 ~b:0.5
              | [] -> Style.running_fiber cr
            end;
            C.rectangle cr ~x:x0 ~y:(y +. 10.) ~w ~h:14.0;
            C.fill cr;
            let label op =
              let clip_area = (w -. 2.0, v.height) in
              C.paint_text cr ~x:(x0 +. 2.) ~y:(y +. 22.) op
                ~clip_area
            in
            begin match stack with
              | `Suspend op :: _ -> C.set_source_rgb cr ~r:1.0 ~g:1.0 ~b:1.0; label op
              | `Span op :: p ->
                C.set_source_rgb cr ~r:0.0 ~g:0.0 ~b:0.0;
                if p == !prev_stack then label op
              | [] -> ()
            end;
            prev_stack := stack
          );
      );
      render_events v cr f
    )

  and render_cc v cr start_time (cc : Model.item) ty =
    render_events v cr cc;
    let label = Option.value cc.name ~default:ty in
    let x = View.x_of_time v start_time in
    let y = y_of_row v cc.y in
    let w =
      match cc.end_time with
      | None -> v.width -. x
      | Some stop -> View.x_of_time v stop -. x
    in
    let h = float cc.height *. Style.line_spacing -. 4. in
    C.set_source_rgb cr ~r:0.0 ~g:0.0 ~b:0.0;
    draw_l_bracket cr ~x ~y ~w ~h; 
    draw_r_bracket cr ~x:(x +. w) ~y ~w ~h; 
    C.set_font_size cr Style.small_text;
    let clip_width =
      match cc.end_cc_label with
      | Some t -> View.x_of_time v t -. x
      | None -> w
    in
    C.paint_text cr ~x:(x +. 2.) ~y:(y +. 8.) ~clip_area:(clip_width -. 2., v.height) label

  let iter_gc_spans (t0, t1) fn ring =
    let arr = ring.Model.Ring.events in
    (* todo: binary search *)
    for i = 0 to Array.length arr - 1 do
      let time, e = arr.(i) in
      if time >= t0 && time < t1 then fn (time, e)
    done;
    fn (t1, [])

  let render_gc_events v cr ring (cc : Model.item) (t0, t1) =
    let y = y_of_row v cc.y in
    let h = float cc.height *. Style.line_spacing in
    let t1 = Option.value t1 ~default:v.View.model.duration in
    let prev_stack = ref [] in
    let event = ref (t0, []) in
    C.set_font_size cr Style.small_text;
    ring |> iter_gc_spans (t0, t1) (fun event' ->
        let t0, stack = !event in
        event := event';
        let t1 = fst event' in
        let x0 = View.x_of_time v t0 in
        let x1 = View.x_of_time v t1 in
        let w = x1 -. x0 in
        begin match stack with
          | [] -> ()
          | op :: p ->
            let g = min 1.0 (0.1 *. float (List.length stack)) in
            C.set_source_rgba cr ~r:g ~g:g ~b:(g /. 2.) ~a:0.9;
            C.rectangle cr ~x:x0 ~y ~w ~h;
            C.fill cr;
            if p == !prev_stack then (
              let clip_area = (w -. 0.2, v.height) in
              if g < 0.5 then C.set_source_rgb cr ~r:1.0 ~g:1.0 ~b:1.0
              else C.set_source_rgb cr ~r:0.0 ~g:0.0 ~b:0.0;
              C.paint_text cr ~x:(x0 +. 2.) ~y:(y +. 8.) op
                ~clip_area
            )
        end;
        prev_stack := stack
      )

  let render_domain v cr start_time ring_id (cc : Model.item) =
    let ring = Model.ring v.View.model ring_id in
    render_gc_events v cr ring cc (start_time, cc.end_time);
    render_events v cr cc

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
    let ring_id, cc = v.model.root in
    render_domain v cr 0.0 ring_id cc
end
