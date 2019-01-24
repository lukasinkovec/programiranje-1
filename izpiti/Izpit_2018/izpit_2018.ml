(*1.naloga*)

(*a) primer*)

let podvoji_vsoto x y = 2 * (x + y)

(*b) primer*)

let povsod_vecji (x1, x2, x3) (y1, y2, y3) = (x1 > y1) && (x2 > y2) && (x3 > y3)

(*c) primer*)

type 'a option = None | Some of 'a

let uporabi_ce_lahko f = function
  | None -> None
  | Some x -> Some (f x)

(*d) primer*)

let pojavi_dvakrat x xs =
  let rec aux acc = function
    | [] -> acc = 2
    | y :: ys ->
      if x = y then
        let new_acc = acc + 1 in
        aux new_acc ys
      else
        aux acc ys
  in
  aux 0 xs

(*e) primer*)

let izracunaj_v_tocki x xs =
  let rec aux acc = function
    | [] -> List.rev acc
    | f :: fs ->
      let new_acc = (f x) :: acc in
      aux new_acc fs
  in
  aux [] xs

(*f) primer*)

let eksponent x p =
  let rec aux acc = function
    | 0 -> acc
    | n ->
      let new_acc = x * acc in
      aux new_acc (n - 1)
  in
  aux 1 p


(*2.naloga*)

(*a) primer*)

type 'a mm_drevo =  Prazno | Veja of 'a mm_drevo * ('a * int) * 'a mm_drevo

(*b) primer*)

let veja x = Veja (Prazno, (x, 1), Prazno)

let rec vstavi drevo x =
  match drevo with
  | Prazno -> veja x
  | Veja (lt, (y, n), rt) -> 
    if x = y then
      Veja (lt, (y , (n + 1)), rt)
    else if x > y then
      Veja (lt, (y, n), vstavi rt x)
    else
      Veja (vstavi lt x, (y, n), rt )

(*c) primer*)

let multimnozica_iz_seznama xs =
  let rec aux acc = function
    | [] -> acc
    | y :: ys ->
      let new_acc = vstavi acc y in
      aux new_acc ys
  in
  aux Prazno xs

let test = multimnozica_iz_seznama [2; 5; 1; 4; 1; 1; 2; 8; 8]
   
(*d) primer*)

let rec velikost_multimnozice = function
  | Prazno -> 0
  | Veja (lt, (_, n), rt) -> n + velikost_multimnozice lt + velikost_multimnozice rt

(*e) primer*)

let dodaj n x xs =
  let rec aux acc = function
    | 0 -> acc
    | k ->
      let new_acc = x :: acc in
      aux new_acc (k - 1)
  in
  aux xs n

let rec seznam_iz_multimnozice = function
  | Prazno -> []
  | Veja (lt, (x, n), rt) -> (seznam_iz_multimnozice lt) @ (dodaj n x (seznam_iz_multimnozice rt))