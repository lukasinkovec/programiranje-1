(* ========== Vaja 8: Moduli  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
"Once upon a time, there was a university with a peculiar tenure policy. All
 faculty were tenured, and could only be dismissed for moral turpitude. What
 was peculiar was the definition of moral turpitude: making a false statement
 in class. Needless to say, the university did not teach computer science.
 However, it had a renowned department of mathematics.

 One Semester, there was such a large enrollment in complex variables that two
 sections were scheduled. In one section, Professor Descartes announced that a
 complex number was an ordered pair of reals, and that two complex numbers were
 equal when their corresponding components were equal. He went on to explain
 how to convert reals into complex numbers, what "i" was, how to add, multiply,
 and conjugate complex numbers, and how to find their magnitude.

 In the other section, Professor Bessel announced that a complex number was an
 ordered pair of reals the first of which was nonnegative, and that two complex
 numbers were equal if their first components were equal and either the first
 components were zero or the second components differed by a multiple of 2π. He
 then told an entirely different story about converting reals, "i", addition,
 multiplication, conjugation, and magnitude.

 Then, after their first classes, an unfortunate mistake in the registrar's
 office caused the two sections to be interchanged. Despite this, neither
 Descartes nor Bessel ever committed moral turpitude, even though each was
 judged by the other's definitions. The reason was that they both had an
 intuitive understanding of type. Having defined complex numbers and the
 primitive operations upon them, thereafter they spoke at a level of
 abstraction that encompassed both of their definitions.

 The moral of this fable is that:
   Type structure is a syntactic discipline for enforcing levels of
   abstraction."

 from:
 John C. Reynolds, "Types, Abstraction, and Parametric Polymorphism", IFIP83
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*----------------------------------------------------------------------------*]
 Definirajte signaturo [NAT], ki določa strukturo naravnih števil. Ima osnovni 
 tip, funkcijo enakosti, ničlo in enko, seštevanje, odštevanje in množenje.
 Hkrati naj vsebuje pretvorbe iz in v OCamlov [int] tip.

 Opomba: Funkcije za pretvarjanje ponavadi poimenujemo [to_int] and [of_int],
 tako da skupaj z imenom modula dobimo ime [NAT.of_int], ki nam pove, da 
 pridobivamo naravno število iz celega števila.
[*----------------------------------------------------------------------------*)

module type NAT = sig
  type t

  val eq   : t -> t -> bool
  val zero : t
  val one  : t
  val add  : t -> t -> t
  val sub  : t -> t -> t
  val mult : t -> t -> t
  val to_int : t -> int
  val of_int : int -> t
end

(*----------------------------------------------------------------------------*]
 Napišite implementacijo modula [Nat_int], ki zgradi modul s signaturo [NAT],
 kjer kot osnovni tip uporablja OCamlov tip [int].

 Namig: Dokler ne implementirate vse funkcij v [Nat_int] se bo OCaml pritoževal.
 Temu se lahko izognete tako, da funkcije, ki še niso napisane nadomestite z 
 [failwith "later"], vendar to ne deluje za konstante.
[*----------------------------------------------------------------------------*)

module Nat_int : NAT = struct
  type t = int

  let eq x y = (x = y)
  let zero = 0
  let one = 1
  let add x y = x + y 
  let sub x y = x - y
  let mult x y = x * y
  let to_int x = x
  let of_int x = x
end

(*----------------------------------------------------------------------------*]
 Napišite implementacijo [NAT], ki temelji na Peanovih aksiomih:
 https://en.wikipedia.org/wiki/Peano_axioms
   
 Osnovni tip modula definirajte kot vsotni tip, ki vsebuje konstruktor za ničlo
 in konstruktor za naslednika nekega naravnega števila.
 Večino funkcij lahko implementirate s pomočjo rekurzije. Naprimer, enakost
 števil [k] in [l] določimo s hkratno rekurzijo na [k] in [l], kjer je osnoven
 primer [Zero = Zero].

[*----------------------------------------------------------------------------*)

module Nat_peano : NAT = struct
  type t = Zero | Succ of t

  let rec eq x y = 
    match x, y with
    | Zero, Zero -> true
    | Zero, _ | _, Zero -> false
    | Succ x', Succ y' -> eq x' y'
  
  let zero = Zero
  let one = Succ Zero

  let rec add x y =
    match x, y with
    | Zero, Zero -> zero
    | Zero, x' | x', Zero -> x'
    | Succ x', Succ y' -> add x' (Succ (Succ y'))

  let rec sub x y =
    match x, y with
    | Zero, _ -> zero
    | x', Zero -> x'
    | Succ x', Succ y' -> sub x' y'

  let rec mult x y =
    match x, y with
    | Zero, _ | _, Zero -> zero
    | Succ Zero, x' | x', Succ Zero -> x'
    | Succ x', Succ y' -> add y' (mult x' (Succ y'))

  let rec to_int = function
    | Zero -> 0
    | Succ x -> 1 + to_int x

  let rec of_int = function
    | 0 -> zero
    | x -> Succ (of_int (x - 1))
  
end

(*----------------------------------------------------------------------------*]
 V OCamlu lahko module podajamo kot argumente funkcij, z uporabo besede
 [module]. Funkcijo, ki sprejme modul torej definiramo kot

 # let f (module M : M_sig) = ...

 in ji podajamo argumente kot 

 # f (module M_implementation);;

 Funkcija [sum_nat_100] sprejme modul tipa [NAT] in z uporabo modula sešteje
 prvih 100 naravnih števil. Ker funkcija ne more vrniti rezultata tipa [NAT.t]
 (saj ne vemo, kateremu od modulov bo pripadal, torej je lahko [int] ali pa
  variantni tip) na koncu vrnemo rezultat tipa [int] z uporabo metode [to_int].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # sum_nat_100 (module Nat_int);;
 - : int = 4950
 # sum_nat_100 (module Nat_peano);;
 - : int = 4950
[*----------------------------------------------------------------------------*)

let sum_nat_100 (module Nat : NAT) =
  let rec sum_nat_100' acc1 acc2 = function
    | 0 -> Nat.to_int acc2
    | x ->
      let new_acc2 = (Nat.add acc1 acc2) in
      let new_acc1 = (Nat.add Nat.one acc1) in
      sum_nat_100' new_acc1 new_acc2 (x - 1)
  in
  sum_nat_100' Nat.zero Nat.zero 100

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Now we follow the fable told by John Reynolds in the introduction.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte signaturo modula kompleksnih števil.
 Potrebujemo osnovni tip, test enakosti, ničlo, enko, imaginarno konstanto i,
 negacijo, konjugacijo, seštevanje in množenje. 
[*----------------------------------------------------------------------------*)

module type COMPLEX = sig
  type t

  val eq   : t -> t -> bool
  val zero : t
  val one  : t
  val i    : t
  val neg  : t -> t
  val con  : t -> t
  val add  : t -> t -> t
  val mult : t -> t -> t
end

(*----------------------------------------------------------------------------*]
 Napišite kartezično implementacijo kompleksnih števil, kjer ima vsako
 kompleksno število realno in imaginarno komponento.
[*----------------------------------------------------------------------------*)

module Cartesian : COMPLEX = struct
  type t = {re : float; im : float}

  let eq x y = (x.re = y.re) && (x.im = y.im)
  let zero = {re = 0.; im = 0.}
  let one = {re = 1.; im = 0.}
  let i = {re = 0.; im = 1.}
  let neg x = {re = (-. x.re); im = (-. x.im)}
  let con x = {re = x.re; im = (-. x.im)}
  let add x y = {re = x.re +. y.re; im = x.im +. y.im} 
  let mult x y = {re = (x.re *. y.re) -. (x.im *. y.im); im = (x.re *. y.im) +. (x.im *. y.re)}
end

(*----------------------------------------------------------------------------*]
 Sedaj napišite še polarno implementacijo kompleksnih števil, kjer ima vsako
 kompleksno število radij in kot (angl. magnitude in argument).
   
 Priporočilo: Seštevanje je v polarnih koordinatah zahtevnejše, zato si ga 
 pustite za konec (lahko tudi za konec stoletja).
[*----------------------------------------------------------------------------*)

module Polar : COMPLEX = struct
  type t = {magn : float; arg : float}

  let pi = 2. *. acos 0.
  let rad_of_deg deg = (deg /. 180.) *. pi
  let deg_of_rad rad = (rad /. pi) *. 180.

  let rec convert x =
    if x < 0. then
      convert (x +. 360.)
    else if x < 360. then
      x
    else
      convert (x -. 360.)

  let eq x y = (x.magn = y.magn) && ((convert x.arg) = (convert y.arg))
  let zero = {magn = 0.; arg = 0.}
  let one = {magn = 1.; arg = 0.}
  let i = {magn = 1.; arg = 90.}
  let neg x = {magn = x.magn; arg = convert (x.arg +. 180.)}
  let con x = {magn = x.magn; arg = convert (-. x.arg)}

  let add x y =
    let r1 = x.magn in
    let r2 = y.magn in
    let fi1 = (convert x.arg) in
    let fi2 = (convert y.arg) in
    let r = (sqrt (r1 *. r1 +. r2 *. r2 +. 2. *. r1 *. r2 *. cos ((rad_of_deg fi2) -. (rad_of_deg fi1)))) in
    let left = (r2 *. sin ((rad_of_deg fi2) -. (rad_of_deg fi1))) in
    let right = (r1 +. r2 *. cos ((rad_of_deg fi2) -. (rad_of_deg fi1))) in
    let final = (atan (left /. right)) in
    if eq x zero then
      y
    else if eq y zero then
      x
    else if right > 0. then
      {magn = r; arg = convert (fi1 +. (deg_of_rad final))}
    else if right = 0. then
      if left > 0. then
        {magn = r; arg = convert (fi1 +. 90.)}
      else 
        {magn = r; arg = convert (fi1 -. 90.)}
    else if left >= 0. then
      {magn = r; arg = convert (fi1 +. (deg_of_rad final) +. 180.)}
    else
      {magn = r; arg = convert (fi1 +. (deg_of_rad final) -. 180.)}

  let mult x y = {magn = x.magn *. y.magn; arg = convert (x.arg +. y.arg)}
end

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Na vajah z iskalnimi drevesi smo definirali tip slovarjev 
 [('key, 'value) dict], ki je implementiral [dict_get], [dict_insert] in
 [print_dict]. Napišite primerno signaturo za slovarje [DICT] in naredite
 implementacijo modula z drevesi (kot na prejšnjih vajah). 
 
 Modul naj vsebuje prazen slovar [empty] in pa funkcije [get], [insert] in
 [print] (print naj ponovno deluje zgolj na [(string, int) t].
[*----------------------------------------------------------------------------*)

module type DICT = sig
  type ('key, 'value) dict
  type 'value option

  val leaf_dict : 'key * 'value -> ('key, 'value) dict
  val empty  : ('key, 'value) dict
  val none   : 'value option
  val vopt_v : 'value option -> 'value
  val get    : 'key -> ('key, 'value) dict -> 'value option
  val insert : 'key -> 'value -> ('key, 'value) dict -> ('key, 'value) dict
  val print  : (string, int) dict -> unit
end

module Tree_dict : DICT = struct
  type ('key, 'value) dict = Empty | Dict of ('key, 'value) dict * ('key * 'value) * ('key, 'value) dict
  type 'value option = None | Some of 'value

  let leaf_dict x = Dict (Empty, x, Empty)
  let empty = Empty
  let none = None
  let vopt_v (Some x) = x

  let rec get key = function
    | Empty -> None
    | Dict (ld, (k, v), rd) -> 
      if key = k then
        Some v
      else if key > k then
        get key rd
      else
        get key ld

  let rec insert key value = function
    | Empty -> leaf_dict (key, value)
    | Dict (ld, (k, v), rd) -> 
      if key = k then
        Dict (ld, (k, value), rd)
      else if key > k then
        Dict (ld, (k, v), insert key value rd)
      else
        Dict (insert key value ld, (k, v), rd)

  let rec print = function
    | Empty -> ()
    | Dict (ld, (k, v), rd) -> 
      print ld;
      print_string (k ^ " : ");
      print_int v;
      print_endline "";
      print rd

end

(*----------------------------------------------------------------------------*]
 Funkcija [count (module Dict) list] prešteje in izpiše pojavitve posameznih
 elementov v seznamu [list] s pomočjo izbranega modula slovarjev.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count (module Tree_dict) ["b"; "a"; "n"; "a"; "n"; "a"];;
 a : 3
 b : 1
 n : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let count (module Dict : DICT) list =
  let rec count' acc = function
    | [] -> acc
    | x :: xs -> 
      if (Dict.get x acc) = Dict.none then
        let new_acc = (Dict.insert x 1 acc) in
        count' new_acc xs
      else
        let new_acc = (Dict.insert x ((Dict.vopt_v (Dict.get x acc)) + 1) acc) in
        count' new_acc xs
  in Dict.print (count' Dict.empty list)
