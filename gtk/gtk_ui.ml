open Eio_trace

let ( ==> ) signal callback =
  ignore (signal ~callback : GtkSignal.id)

let (let*) = Result.bind

let ui_xml = {|
<ui>
  <popup name='PopupMenu'>
    <menuitem action='export-image' />
    <menuitem action='viewport-set-start' />
    <menuitem action='viewport-set-duration' />
  </popup>
</ui>
|}

let create ~title tracefile =
  let last_export = ref None in
  let actions = GAction.action_group ~name:"main" () in
  let ui = GAction.ui_manager () in
  ui#insert_action_group actions 0;
  let _id : GAction.ui_id = ui#add_ui_from_string ui_xml in

  let window = GWindow.window () in
  window#add_accel_group ui#get_accel_group;

  window#set_title title;
  window#event#connect#delete ==> (fun _ -> GMain.quit (); true);
  let table = GPack.table ~rows:3 ~columns:2 ~homogeneous:false ~packing:window#add () in
  let hadjustment = GData.adjustment () in
  let vadjustment = GData.adjustment () in
  let area = GMisc.drawing_area ~packing:(table#attach ~left:0 ~top:0 ~expand:`BOTH ~fill:`BOTH) () in
  let _hscroll = GRange.scrollbar `HORIZONTAL ~adjustment:hadjustment ~packing:(table#attach ~left:0 ~top:1 ~expand:`X ~fill:`BOTH) () in
  let _vscroll = GRange.scrollbar `VERTICAL ~adjustment:vadjustment ~packing:(table#attach ~left:1 ~top:0 ~expand:`Y ~fill:`BOTH) () in
  let minibuffer = Minibuffer.create ~packing:(table#attach ~left:0 ~top:2 ~right:2 ~fill:`BOTH) () in
  let v =
    let layout = Layout.load tracefile in
    View.of_layout layout ~width:1000. ~height:1000.
  in
  let set_scollbars () =
    let (xlo, xhi, xsize, xvalue), (ylo, yhi, ysize, yvalue) = View.scroll_bounds v in
    hadjustment#set_bounds ~lower:xlo ~upper:xhi ~page_size:xsize ();
    vadjustment#set_bounds ~lower:ylo ~upper:yhi ~page_size:ysize ();
    hadjustment#set_value xvalue;
    vadjustment#set_value yvalue;
  in
  let set_start_time t =
    View.set_start_time v t
    |> hadjustment#set_value
  in
  let set_scroll_y t =
    View.set_scroll_y v t
    |> vadjustment#set_value
  in
  area#misc#connect#size_allocate ==> (fun alloc ->
      View.set_size v (float_of_int alloc.Gtk.width) (float_of_int alloc.Gtk.height);
      set_scollbars ()
    );
  let redraw () = GtkBase.Widget.queue_draw area#as_widget in
  area#misc#connect#draw ==> (fun cr ->
      let alloc = area#misc#allocation in
      v.width <- float alloc.width;
      v.height <- float alloc.height;
      Cairo.set_font_size cr 16.;
      Cairo.select_font_face cr "Sans";
      Cairo.set_line_join cr Cairo.JOIN_BEVEL;
      Render_cairo.render v cr;
      true
    );
  area#misc#set_app_paintable true;

  let show_start () =
    let current = View.time_of_x v 0. /. 1e9 in 
    Minibuffer.show minibuffer
      ~label:"Viewport start: "
      ~value:(Time.to_string current)
      (fun s ->
         match Time.of_string s with
         | Ok time ->
           set_start_time (time *. 1e9);
           redraw ();
           Minibuffer.hide minibuffer
         | Error message ->
           let box =
             GWindow.message_dialog ()
               ~message
               ~parent:window
               ~buttons:GWindow.Buttons.close
           in
           box#connect#response ==> (fun _ -> box#destroy ());
           box#show ()
      )
  in

  let show_duration () =
    let current = View.get_duration v /. 1e9 in
    Minibuffer.show minibuffer
      ~label:"Viewport duration: "
      ~value:(Time.to_string current)
      (fun s ->
         match Time.of_string s with
         | Ok d ->
           View.set_duration v (d *. 1e9);
           redraw ();
           Minibuffer.hide minibuffer
         | Error message ->
           let box =
             GWindow.message_dialog ()
               ~message
               ~parent:window
               ~buttons:GWindow.Buttons.close
           in
           box#connect#response ==> (fun _ -> box#destroy ());
           box#show ()
      )
  in

  let alert message =
    let dialog = GWindow.message_dialog ~title:"Error" ~buttons:GWindow.Buttons.ok ~message () in
    dialog#show ();
    match dialog#run () with
    | `DELETE_EVENT | `OK -> dialog#destroy ()
  in

  let save_image () =
    let dialog = GWindow.file_chooser_dialog ~title:"Export image" ~action:`SAVE ~parent:window () in
    begin match !last_export with
      | None ->
        let dir = Filename.dirname tracefile in
        ignore (dialog#set_current_folder dir : bool);
        dialog#set_current_name ((Filename.basename tracefile |> Filename.remove_extension) ^ ".svg")
      | Some path ->
        ignore (dialog#set_filename path : bool)
    end;
    dialog#add_select_button_stock `SAVE `OK;
    dialog#set_default_response `OK;
    dialog#add_button_stock `CANCEL `CANCEL;
    dialog#connect#response ==> (function
        | `DELETE_EVENT | `CANCEL ->
          dialog#destroy ()
        | `OK ->
          match
            match dialog#get_filenames with
            | [path] ->
              let* fmt = Save.format_of_string (Filename.extension path) in
              last_export := Some path;
              Save.export_image v fmt path
            | _ -> Error "Must select one path"
          with
          | Ok () -> dialog#destroy ()
          | Error msg -> alert msg
      );
    dialog#show ();
  in

  GAction.add_action "export-image" actions
    ~label:"Export image as..."
    ~accel:"<control>e"
    ~stock:`SAVE_AS
    ~callback:(fun _a -> save_image ());

  GAction.add_action "viewport-set-start" actions
    ~label:"Set start time..."
    ~callback:(fun _a -> show_start ());

  GAction.add_action "viewport-set-duration" actions
    ~label:"Set duration..."
    ~callback:(fun _a -> show_duration ());

  window#event#connect#key_press ==> (fun ev ->
      let keyval = GdkEvent.Key.keyval ev in
      if keyval = GdkKeysyms._F5 then (
        let layout = Layout.load tracefile in
        View.set_layout v layout;
        set_scollbars ();
        redraw ();
        true
      ) else if Minibuffer.is_open minibuffer then (
        if keyval = GdkKeysyms._Escape then (
          Minibuffer.hide minibuffer;
          true
        ) else false
      ) else (
        if GdkEvent.Key.state ev <> [] then false
        else if keyval = GdkKeysyms._s then (
          show_start ();
          true
        ) else if keyval = GdkKeysyms._d then (
          show_duration ();
          true
        ) else false
      )
    );

  area#event#add [`SMOOTH_SCROLL; `BUTTON1_MOTION; `BUTTON_PRESS];
  area#event#connect#scroll ==> (fun ev ->
      let x = GdkEvent.Scroll.x ev in
      let t_at_pointer = View.time_of_x v x in
      let redraw_zoomed () =
        let t_new_at_pointer = View.time_of_x v x in
        set_start_time (v.start_time -. (t_new_at_pointer -. t_at_pointer));
        redraw ()
      in
      let zoom diff =
        View.zoom v diff;
        set_scollbars ();
        redraw_zoomed ();
        true
      in
      match GdkEvent.Scroll.direction ev with
      | `SMOOTH -> zoom ((-. GdkEvent.Scroll.delta_y ev) /. 8.0)
      | _ -> false
    );

  let menu = ui#get_widget "/PopupMenu" in
  let menu = new GMenu.menu (Gobject.try_cast menu#as_widget "GtkMenu") in
  GtkMenuProps.Menu.attach_to_widget menu#as_menu window#as_widget;

  let drag_start = ref None in
  area#event#connect#button_press ==> (fun ev ->
      match GdkEvent.get_type ev, GdkEvent.Button.button ev with
      | `BUTTON_PRESS, 1 ->
        let start_t = View.time_of_x v (GdkEvent.Button.x ev) in
        drag_start := Some (start_t, v.scroll_y +. GdkEvent.Button.y ev);
        true
      | `BUTTON_PRESS, (3 as button) ->
        menu#popup ~button ~time:(GdkEvent.Button.time ev);
        true
      | _ -> false
    );

  area#event#connect#motion_notify ==> (fun ev ->
      match !drag_start with
      | None -> false
      | Some (start_time, start_y) ->
        let x = GdkEvent.Motion.x ev in
        let y = GdkEvent.Motion.y ev in
        let old_pos = v.start_time, v.scroll_y in
        set_start_time (start_time -. View.timespan_of_width v x);
        set_scroll_y (start_y -. y);
        let new_pos = v.start_time, v.scroll_y in
        if old_pos <> new_pos then redraw ();
        true
    );

  hadjustment#connect#value_changed ==> (fun () ->
      set_start_time (View.timespan_of_width v hadjustment#value);
      redraw ();
    );

  vadjustment#connect#value_changed ==> (fun () ->
      set_scroll_y vadjustment#value;
      redraw ();
    );

  let height =
    int_of_float @@ min
      (float (Gdk.Screen.height ()) *. 0.8)
      ((float v.layout.height +. 1.) *. View.pixels_per_row +. 2. *. View.v_margin)
  in
  window#set_default_size
    ~width:(int_of_float (float (Gdk.Screen.width ()) *. 0.8))
    ~height;

  (* GTK fails to display the scrollbars correctly for some reason
     (possibly because Sway sends two size allocations in quick succession?),
     so force a recalculation after a short delay.
     Also, delay zooming to fit to give Sway time to sort itself out. *)
  let fix_scrollbars () =
    View.zoom_to_fit v;
    set_scollbars ();
    false
  in
  ignore (GMain.Timeout.add ~ms:200 ~callback:fix_scrollbars : Glib.Timeout.id);

  window#show ()
