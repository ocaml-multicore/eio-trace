(* An (Augmented) Interval Tree:
  https://en.wikipedia.org/wiki/Interval_tree#Augmented_tree *)

type 'a interval = {
  start : float;
  stop : float;
  value : 'a;
}

let compare_start a b = compare a.start b.start

type 'a tree =
  | Empty
  | Node of {
      v : 'a interval;
      left : 'a tree;     (* No interval in the left sub-tree starts after v starts. *)
      right : 'a tree;
      mutable subtree_stop : float; (* The max end_time in this sub-tree *)
    }

let pp_interval f { start; stop; value = _ } =
  Fmt.pf f "[%g, %g)" start stop

let rec dump f = function
  | Empty -> Fmt.string f "."
  | Node { left; right; subtree_stop; v } ->
    Fmt.pf f "@[<v2>%a subtree_stop = %g@,left = %a@,right = %a@]"
      pp_interval v
      subtree_stop
      dump left
      dump right
      
let max_stop n acc =
  match n with
  | Empty -> acc
  | Node n -> max acc n.subtree_stop

let rec tree_of_slice arr i len =
  if len = 0 then Empty
  else (
    let left_len = len / 2 in
    let mid = i + left_len in
    let v = arr.(mid) in
    let left = tree_of_slice arr i left_len in
    let right = tree_of_slice arr (mid + 1) (len - left_len  - 1) in
    let subtree_stop =
      v.stop
      |> max_stop left
      |> max_stop right
    in
    Node { v; left; right; subtree_stop }
  )

let create spans =
  let spans = Array.of_list spans in
  Array.sort compare_start spans;
  tree_of_slice spans 0 (Array.length spans)

let overlaps i start stop =
  start < i.stop && stop > i.start

let rec iter_overlaps f start stop = function
  | Empty -> ()
  | Node { v; left; right; subtree_stop } ->
    if subtree_stop > start then (
      (* Search the left sub-tree. *)
      iter_overlaps f start stop left;
      (* Check this node. *)
      if overlaps v start stop then f v.value;
      (* Search the right sub-tree. *)
      if v.start < stop then
        iter_overlaps f start stop right
    )
