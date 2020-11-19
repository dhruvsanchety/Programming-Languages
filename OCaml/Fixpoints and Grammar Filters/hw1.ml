let rec exist x a = match a with
  [] -> false
  | head :: other -> if x = head then true else exist x other

let rec subset a b = match a with
  [] -> true
  | head::other -> if exist head b then subset other b else false

let equal_sets a b = subset a b && subset b a

let rec set_union a b = match a with
  [] -> b
  | head::other -> if exist head b then set_union other b else set_union other (head::b)

let set_intersection a b = List.filter (fun x -> exist x a) b

let set_diff a b = List.filter (fun x -> not (exist x b)) a

let rec computed_fixed_point eq f x = if eq (f x) x then x else computed_fixed_point eq f (f x)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let filter_reachable g =

  let rec search previous =

    let temporary = List.filter(fun x -> if exist x previous then true
    else List.exists (fun y -> List.exists (function | T _ -> false | N z -> z = (fst x)) (snd y)) previous ) (snd g) in    
    if previous = temporary then previous else search temporary in

  fst g, search ( List.filter(fun x -> fst x = fst g) (snd g) )