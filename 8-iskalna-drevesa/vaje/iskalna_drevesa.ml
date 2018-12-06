(* ========== Vaja 4: Iskalna Drevesa  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a tree =  Empty | Node of 'a tree * 'a * 'a tree

(*----------------------------------------------------------------------------*]
 Definirajmo si testni primer za preizkušanje funkcij v nadaljevanju. Testni
 primer predstavlja spodaj narisano drevo, pomagamo pa si s pomožno funkcijo
 [leaf], ki iz podatka zgradi list.
          5
         / \
        2   7
       /   / \
      0   6   11
[*----------------------------------------------------------------------------*)

let leaf x = Node (Empty, x, Empty)

let test_tree = 
  let lt = Node (leaf 0, 2, Empty) in
  let rt = Node (leaf 6, 7, leaf 11) in
  Node (lt, 5, rt)

(*----------------------------------------------------------------------------*]
 Funkcija [mirror] vrne prezrcaljeno drevo. Na primeru [test_tree] torej vrne
          5
         / \
        7   2
       / \   \
      11  6   0
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mirror test_tree ;;
 - : int tree =
 Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
 Node (Empty, 2, Node (Empty, 0, Empty)))
[*----------------------------------------------------------------------------*)

let rec mirror = function
  | Empty -> Empty
  | Node (lt, x, rt) -> Node (mirror rt, x, mirror lt)

(*----------------------------------------------------------------------------*]
 Funkcija [height] vrne višino oz. globino drevesa, funkcija [size] pa število
 vseh vozlišč drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # height test_tree;;
 - : int = 3
 # size test_tree;;
 - : int = 6
[*----------------------------------------------------------------------------*)

let rec height = function
  | Empty -> 0
  | Node (lt, _, rt) -> 1 + max (height lt) (height rt)

let rec size = function
  | Empty -> 0
  | Node (lt, _, rt) -> 1 + size lt + size rt

let tl_rec_size tree =
  let rec tl_rec_size' acc queue = 
    match queue with
    | [] -> acc
    | t :: ts -> (
      match t with
      | Empty -> tl_rec_size' acc ts
      | Node (lt, _, rt) -> 
        let new_acc = acc + 1 in
        let new_queue = lt :: rt :: ts in
        tl_rec_size' new_acc new_queue
    )
    in
    tl_rec_size' 0 [tree]

(*----------------------------------------------------------------------------*]
 Funkcija [map_tree f tree] preslika drevo v novo drevo, ki vsebuje podatke
 drevesa [tree] preslikane s funkcijo [f].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # map_tree ((<)3) test_tree;;
 - : bool tree =
 Node (Node (Node (Empty, false, Empty), false, Empty), true,
 Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)

let rec map_tree f = function
  | Empty -> Empty
  | Node (lt, x, rt) -> Node (map_tree f lt, f x, map_tree f rt)

(*----------------------------------------------------------------------------*]
 Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)

let rec list_of_tree = function
  | Empty -> []
  | Node (lt, x, rt) -> list_of_tree lt @ [x] @ list_of_tree rt

(*----------------------------------------------------------------------------*]
 Funkcija [is_bst] preveri ali je drevo binarno iskalno drevo (Binary Search 
 Tree, na kratko BST). Predpostavite, da v drevesu ni ponovitev elementov, 
 torej drevo npr. ni oblike Node( leaf 1, 1, leaf 2)). Prazno drevo je BST.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_bst test_tree;;
 - : bool = true
 # test_tree |> mirror |> is_bst;;
 - : bool = false
[*----------------------------------------------------------------------------*)

type 'a option = None | Some of 'a

let is_sorted list = 
    let rec is_sorted' acc = function
      | [] -> true
      | x :: xs -> 
        if Some x >= acc then
          let new_acc = Some x in
          is_sorted' new_acc xs
        else
          false
    in
    is_sorted' None list

let is_bst tree = is_sorted (list_of_tree tree)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 V nadaljevanju predpostavljamo, da imajo dvojiška drevesa strukturo BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert] v iskalno drevo pravilno vstavi dani element. Funkcija 
 [member] preveri ali je dani element v iskalnem drevesu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 2 (leaf 4);;
 - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
 # member 3 test_tree;;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec insert x = function
  | Empty -> leaf x
  | Node (lt, y, rt) -> 
    if x > y then
      Node (lt, y, insert x rt)
    else
      Node (insert x lt, y, rt )

let rec member x = function
  | Empty -> false
  | Node (lt, y, rt) -> 
    if x = y then
      true
    else if x > y then
      member x rt
    else
      member x lt

(*----------------------------------------------------------------------------*]
 Funkcija [member2] ne privzame, da je drevo bst.
 
 Opomba: Premislte kolikšna je časovna zahtevnost funkcije [member] in kolikšna
 funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n). 
[*----------------------------------------------------------------------------*)

let rec member2 x = function
  | Empty -> false
  | Node (lt, y, rt) -> 
    if x = y then
      true
    else
      (member2 x lt) || (member2 x rt)

(*----------------------------------------------------------------------------*]
 Funkcija [succ] vrne naslednjika korena danega drevesa, če obstaja. Za drevo
 oblike [bst = Node(l, x, r)] vrne najmanjši element drevesa [bst], ki je večji
 od korena [x].
 Funkcija [pred] simetrično vrne največji element drevesa, ki je manjši od
 korena, če obstaja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # succ test_tree;;
 - : int option = Some 6
 # pred (Node(Empty, 5, leaf 7));;
 - : int option = None
[*----------------------------------------------------------------------------*)

let min_tree tree =
  let rec min_tree' acc = function
  | [] -> acc
  | x :: xs -> 
    if acc = None then
      let new_acc = Some x in
      min_tree' new_acc xs
    else
      let new_acc = min (Some x) acc in
      min_tree' new_acc xs
  in
  min_tree' None (list_of_tree tree)

let max_tree tree =
  let rec max_tree' = function
  | [] -> None
  | x :: xs -> max (Some x) (max_tree' xs)
  in
  max_tree' (list_of_tree tree)

let succ = function
  | Empty -> None
  | Node (_, _, rt) -> min_tree rt

let pred = function
  | Empty -> None
  | Node (lt, _, _) -> max_tree lt


(*----------------------------------------------------------------------------*]
 Na predavanjih ste omenili dva načina brisanja elementov iz drevesa. Prvi 
 uporablja [succ], drugi pa [pred]. Funkcija [delete x bst] iz drevesa [bst] 
 izbriše element [x], če ta v drevesu obstaja. Za vajo lahko implementirate
 oba načina brisanja elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # (*<< Za [delete] definiran s funkcijo [succ]. >>*)
 # delete 7 test_tree;;
 - : int tree =
 Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
 Node (Node (Empty, 6, Empty), 11, Empty))
[*----------------------------------------------------------------------------*)

let rec delete_succ x = function
  | Empty -> Empty
  | Node (Empty, y, Empty) ->
    if x = y then
      Empty
    else
      Node (Empty, y, Empty) 
  | Node (lt, y, Empty) ->
    if x = y then
      lt
    else if x > y then
      Node (lt, y, Empty)
    else
      Node (delete_succ x lt, y, Empty) 
  | Node (Empty, y, rt) ->
    if x = y then
      rt
    else if x > y then
      Node (Empty, y, delete_succ x rt) 
    else
      Node (Empty, y, rt)
  | Node (lt, y, rt) -> 
    if x = y then
      let tree = Node (lt, y, rt) in
      let Some y' = (succ tree) in
      Node (lt, y', delete_succ y' rt)
    else if x > y then
      Node (lt, y, delete_succ x rt)
    else
      Node (delete_succ x lt, y, rt)

let rec delete_pred x = function
  | Empty -> Empty
  | Node (Empty, y, Empty) ->
    if x = y then
      Empty
    else
      Node (Empty, y, Empty) 
  | Node (lt, y, Empty) ->
    if x = y then
      lt
    else if x > y then
      Node (lt, y, Empty)
    else
      Node (delete_pred x lt, y, Empty) 
  | Node (Empty, y, rt) ->
    if x = y then
      rt
    else if x > y then
      Node (Empty, y, delete_pred x rt)
    else
      Node (Empty, y, rt) 
  | Node (lt, y, rt) -> 
    if x = y then
      let tree = Node (lt, y, rt) in
      let Some y' = (pred tree) in
      Node (delete_pred y' lt, y', rt)
    else if x > y then
      Node (lt, y, delete_pred x rt)
    else
      Node (delete_pred x lt, y, rt)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI

 S pomočjo BST lahko (zadovoljivo) učinkovito definiramo slovarje. V praksi se
 slovarje definira s pomočjo hash tabel, ki so še učinkovitejše. V nadaljevanju
 pa predpostavimo, da so naši slovarji [dict] binarna iskalna drevesa, ki v
 vsakem vozlišču hranijo tako ključ kot tudi pripadajočo vrednost, in imajo BST
 strukturo glede na ključe. Ker slovar potrebuje parameter za tip ključa in tip
 vrednosti, ga parametriziramo kot [('key, 'value) dict].
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type ('key, 'value) dict = Empty | Dict of ('key, 'value) dict * ('key * 'value) * ('key, 'value) dict

(*----------------------------------------------------------------------------*]
 Napišite testni primer [test_dict]:
      "b":1
      /    \
  "a":0  "d":2
         /
     "c":-2
[*----------------------------------------------------------------------------*)

let leaf_dict x = Dict (Empty, x, Empty)

let test_dict =
  let ld = leaf_dict ("a", 0) in
  let rd = Dict (leaf_dict ("c", -2), ("d", 2), Empty) in
  Dict (ld, ("b", 1), rd)

(*----------------------------------------------------------------------------*]
 Funkcija [dict_get key dict] v slovarju poišče vrednost z ključem [key]. Ker
 slovar vrednosti morda ne vsebuje, vrne [option] tip.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_get "banana" test_dict;;
 - : 'a option = None
 # dict_get "c" test_dict;;
 - : int option = Some (-2)
[*----------------------------------------------------------------------------*)

let rec dict_get key = function
  | Empty -> None
  | Dict (ld, (k, v), rd) -> 
    if key = k then
      Some v
    else if key > k then
      dict_get key rd
    else
      dict_get key ld
      
(*----------------------------------------------------------------------------*]
 Funkcija [print_dict] sprejme slovar s ključi tipa [string] in vrednostmi tipa
 [int] in v pravilnem vrstnem redu izpiše vrstice "ključ : vrednost" za vsa
 vozlišča slovarja.
 Namig: Uporabite funkciji [print_string] in [print_int]. Nize združujemo z
 operatorjem [^]. V tipu funkcije si oglejte, kako uporaba teh funkcij določi
 parametra za tip ključev in vrednosti v primerjavi s tipom [dict_get].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # print_dict test_dict;;
 a : 1
 b : 1
 c : -2
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let rec print_dict = function
  | Empty -> ()
  | Dict (ld, (k, v), rd) -> 
    print_dict ld;
    print_string (k ^ " : ");
    print_int v;
    print_endline "";
    print_dict rd

(*----------------------------------------------------------------------------*]
 Funkcija [dict_insert key value dict] v slovar [dict] pod ključ [key] vstavi
 vrednost [value]. Če za nek ključ vrednost že obstaja, jo zamenja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_insert "1" 14 test_dict |> print_dict;;
 1 : 14
 a : 1
 b : 1
 c : -2
 d : 2
 - : unit = ()
 # dict_insert "c" 14 test_dict |> print_dict;;
 a : 1
 b : 1
 c : 14
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let rec dict_insert key value = function
  | Empty -> leaf_dict (key, value)
  | Dict (ld, (k, v), rd) -> 
    if k = key then
      Dict (ld, (k, value), rd)
    else if key > k then
      Dict (ld, (k, v), dict_insert key value rd)
    else
      Dict (dict_insert key value ld, (k, v), rd)