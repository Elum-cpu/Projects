type 'base sequence = 
  | Empty 
  | Node of 'base * 'base sequence

let rec sort (u: 'base sequence) : 'base sequence =
  match u with
  | Empty -> Empty
  | Node (hd, Empty) -> Node (hd, Empty)
  | _ ->
      let (l, r) = split u in
      let sorted_l = sort l in
      let sorted_r = sort r in
      merge sorted_l sorted_r

let rec split (u: 'base sequence) : ('base sequence * 'base sequence) =
  let rec aux (u: 'base sequence) (l: 'base sequence) (r: 'base sequence) (toggle: bool) : ('base sequence * 'base sequence) =
    match u with
    | Empty -> (l, r)
    | Node (hd, tl) ->
        if toggle then aux tl (Node (hd, l)) r (not toggle) 
        else aux tl l (Node (hd, r)) (not toggle)
  in
  aux u Empty Empty true

let rec merge (l: 'base sequence) (r: 'base sequence) : 'base sequence =
  match (l, r) with
  | (Empty, _) -> r
  | (_, Empty) -> l
  | (Node (hd, tl), Node (hds, tls)) ->
      if hd < hds then Node (hd, merge tl r) 
      else Node (hds, merge l tls)
