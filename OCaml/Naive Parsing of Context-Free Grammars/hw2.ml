type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

type ('nonterminal, 'terminal) symbol =
 | N of 'nonterminal
 | T of 'terminal

let rec exist x a = match a with
  [] -> false
  | head :: other -> if x = head then true else exist x other

let rec set_union a b = match a with
  [] -> b
  | head::other -> if exist head b then set_union other b else set_union other (head::b)

(* Grammar conversion *)
let rec create_rules grammar result symbol =
  if grammar = [] then result
  else if (fst (List.hd grammar)) = symbol then create_rules (List.tl grammar) (List.append result [snd (List.hd grammar)]) symbol
  else create_rules (List.tl grammar) result symbol
;;

let convert_grammar grammar =
  (fst grammar), create_rules (snd grammar) []
;;

(* Parse tree *)
let rec depth_tree tree = match tree with
    | Leaf number -> [number] (*If leaf *)
    | Node (symbol, parse_list) -> breadth_tree parse_list (* If Node *)
      and breadth_tree parse_list =
        if parse_list = [] then [] (* If List is empty, return empty*)
        else ( depth_tree (List.hd parse_list) ) @ ( breadth_tree (List.tl parse_list) ) (*Recurse breadth appended to recurse depth *)      
;;

let parse_tree_leaves tree =
  depth_tree tree
;;

let rec breadth_options grammar option acceptor frag =
  if option = [] then None (* Return None if option = [] *)
  else let attempt = (depth_fragment grammar (List.hd option)) acceptor frag in (* try depth search *)
    if attempt = None then (breadth_options grammar (List.tl option)) acceptor frag (* If depth search yields no result , try braedth *)     
    else attempt (*If it did yield result, return result *)

  and depth_fragment grammar option acceptor frag =
    if option = [] then acceptor frag else if  (*If option = [], then try acceptor on remainder of fragment *)
      frag = [] then None else match option with (*If frag empty, return None *)
        (* If option head = frag head then recurse depth *)
        | (T symbol)::other -> if (List.hd frag) = symbol then (depth_fragment grammar other acceptor (List.tl frag)) else None
        (* If option head is N, recurse breadth *)
        | (N symbol)::other -> breadth_options grammar (grammar symbol) (depth_fragment grammar other acceptor) frag
;;

let make_matcher gram =
  breadth_options (snd gram) ( (snd gram) (fst gram) )
;;

let accept_empty_suffix x =
  if x = [] then Some [] else None

let rec make_parser gram frag = match (parse_breadth_options (snd gram) ((snd gram)(fst gram)) accept_empty_suffix frag) with
  | Some attempt -> let result = breadth [N (fst gram)] attempt in
    if snd result = [] then None else Some (List.hd (snd result))
  | _ -> None

  (*^If parse_breadth_options returns None, return None. Else, if the list is empty, return None, else return root*)
  and breadth option list =
    if option = [] then (list, []) else
      let (a,b) = depth (List.hd option) list in
      let (c,d) = breadth (List.tl option) a in
        (c,b::d)

  and depth option list = match option with
    | (T symbol) -> (list, Leaf symbol)
    | (N symbol) -> if list = [] then ([], Node (symbol, [])) else
      let x = breadth (List.hd list) (List.tl list) in
        (fst x, Node (symbol, snd x))

  and parse_breadth_options grammar option acceptor frag =
    if option = [] then None else
      let attempt = (parse_depth_fragment grammar (List.hd option)) acceptor frag in
      match attempt with
        | None -> (parse_breadth_options grammar (List.tl option)) acceptor frag
        | Some att -> Some ( (List.hd option)::att) (*Only adjustment from previous question, since need to track traversed rules*)

  and parse_depth_fragment grammar option acceptor frag =
    if option = [] then acceptor frag else if
      frag = [] then None else match option with
        | (T symbol)::other -> if (List.hd frag) = symbol then (parse_depth_fragment grammar other acceptor (List.tl frag)) else None        
        | (N symbol)::other -> parse_breadth_options grammar (grammar symbol) (parse_depth_fragment grammar other acceptor) frag
