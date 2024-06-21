let format_of_string = function
  | ".svg" -> Ok `Svg
  | ".png" -> Ok `Png
  | x -> Fmt.error "Unknown format %S (should be .svg or .png)" x

let export_image (v : Eio_trace.View.t) fmt path =
  try
    let create =
      match fmt with
      | `Svg -> Cairo.SVG.create path
      | `Png -> fun ~w ~h -> Cairo.Image.create RGB24 ~w:(int_of_float w) ~h:(int_of_float h)
    in
    let surface =
      create
        ~w:v.width
        ~h:v.height
    in
    let cr = Cairo.create surface in
    Cairo.rectangle cr 0.0 0.0 ~w:v.width ~h:v.height;
    Cairo.clip cr;
    Render_cairo.render v cr;
    begin match fmt with
      | `Svg -> ()
      | `Png -> Cairo.PNG.write surface path
    end;
    Ok (Cairo.Surface.finish surface)
  with ex ->
    Error (Printexc.to_string ex)
