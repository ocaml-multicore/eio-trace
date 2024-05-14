let ( ==> ) signal callback =
  ignore (signal ~callback : GtkSignal.id)

type t = {
  widget : GPack.box;
  label : GMisc.label;
  entry : GEdit.entry;
  mutable action : (string -> unit);
}

let create ~packing () =
  let widget = GPack.hbox ()
      ~packing
      ~border_width:4
      ~show:false
  in
  let label = GMisc.label ~packing:widget#pack ~text:"?" () in
  let entry = GEdit.entry ~packing:(widget#pack ~expand:true) () in
  let t = { widget; label; entry; action = ignore } in
  entry#connect#activate ==> (fun () -> t.action entry#text);
  t

let show t ~label ~value action =
  t.action <- action;
  t.label#set_text label;
  t.entry#set_text value;
  t.widget#misc#show ();
  t.entry#misc#grab_focus ()

let hide t =
  t.action <- ignore;
  t.widget#misc#hide ()

let is_open t =
  t.widget#misc#visible
