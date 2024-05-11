(* Tracks which of a set of rows have free space.
   For large sets it might be more efficient to use an integer tree,
   but layouts that large are fairly unusable anyway. *)

type t = {
  start : int;
  mutable buf : bytes;
}

let create start = { start; buf = Bytes.empty }

let mark t row =
  let i = row - t.start in
  if i >= 0 then (
    let byte = i lsr 3 in
    if byte >= Bytes.length t.buf then (
      let old_buf = t.buf in
      let old_len = Bytes.length old_buf in
      let new_len = max (byte + 1) (old_len * 2) in
      let new_buf = Bytes.extend old_buf 0 (new_len - old_len) in
      Bytes.fill new_buf old_len (new_len - old_len) (Char.chr 0);
      t.buf <- new_buf
    );
    let buf = t.buf in
    let v = Bytes.get_uint8 buf byte lor (1 lsl (i land 0x7)) in
    Bytes.set_uint8 buf byte v
  )

let mark_range t a b =
  let a = max a t.start in
  for i = a to b - 1 do
    mark t i
  done

let rec find_free_bit v start =
  if v land 1 = 0 then start
  else find_free_bit (v lsr 1) (start + 1)

let (.%[]) t row =
  let i = row - t.start in
  let buf = t.buf in
  let byte = i lsr 3 in
  if byte >= Bytes.length buf then false
  else (Bytes.get_uint8 buf byte land (1 lsl (i land 7))) <> 0

let first_free t len =
  assert (len >= 0);
  let rec check i need =
    if need = 0 then i - len
    else if t.%[i] then check (i + 1) len
    else check (i + 1) (need - 1)
  in
  check t.start len
