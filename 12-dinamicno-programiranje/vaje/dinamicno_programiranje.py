#(* ========== Vaje 6: Dinamično programiranje  ========== *)


#(*----------------------------------------------------------------------------*]
# Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
# samo za eno polje navzdol ali za eno polje na desno in na koncu mora prispeti
# v desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
# različne (ne-negativne) mase. Miška bi se rada kar se da nažrla, zato jo
# zanima, katero pot naj ubere.

# Funkcija [max_cheese cheese_matrix], ki dobi matriko [cheese_matrix] z masami
# sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
# optimalni poti.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# # max_cheese test_matrix;;
# - : int = 13
#[*----------------------------------------------------------------------------*)

from functools import lru_cache

test_matrix = [
    [1, 2, 0],
    [2, 4, 5],
    [7, 0, 1]
              ]

def max_cheese(cheese_matrix):
    if cheese_matrix == []:
        return 0
    else:
        m = len(cheese_matrix)
        n = len(cheese_matrix[0])
        @lru_cache(maxsize=None)
        def aux_max_cheese(i, j):
            if (i >= m) or (j >= n):
                return 0
            else:
                right = aux_max_cheese(i, j + 1)
                down = aux_max_cheese(i + 1, j)
                cheese = cheese_matrix[i][j]
                return cheese + max(right, down)
        return aux_max_cheese(0, 0)

#(*let test_matrix = 
#  [| [| 1 ; 2 ; 0 |];
#     [| 2 ; 4 ; 5 |];
#     [| 7 ; 0 ; 1 |] |]

#let memoiziraj f =
#  let rezultati = Hashtbl.create 512 in
#  let rec mem_f x =
#    if Hashtbl.mem rezultati x then
#      Hashtbl.find rezultati x
#    else
#      let y = f mem_f x in
#      Hashtbl.add rezultati x y;
#      y
#  in
#  mem_f

#let max_cheese1 = function
#  | [| |] -> 0
#  | cheese_matrix ->
#    let m = Array.length cheese_matrix in
#    let n = Array.length cheese_matrix.(0) in
#    let rec max_cheese1' i j =
#      if i >= m || j >= n then
#        0
#      else
#        let right = max_cheese1' i (j + 1) in
#        let down = max_cheese1' (i + 1) j in
#        let cheese = cheese_matrix.(i).(j) in
#        cheese + max right down
#    in
#    max_cheese1' 0 0

#let max_cheese2 = function
#  | [| |] -> 0
#  | cheese_matrix -> 
#    let m = Array.length cheese_matrix in
#    let n = Array.length cheese_matrix.(0) in
#    let max_cheese2' rec_max_cheese2' (i, j) =
#      if i >= m || j >= n then
#        0
#      else
#        let right = rec_max_cheese2' (i, (j + 1)) in
#        let down = rec_max_cheese2' ((i + 1), j) in
#        let cheese = cheese_matrix.(i).(j) in
#        cheese + max right down
#    in
#    let mem_max_cheese2' = memoiziraj max_cheese2' in
#    mem_max_cheese2' (0, 0)*)

#(*----------------------------------------------------------------------------*]
# Rešujemo problem sestavljanja alternirajoče obarvanih stolpov. Imamo štiri
# različne tipe gradnikov, dva modra in dva rdeča. Modri gradniki so višin 2 in
# 3, rdeči pa višin 1 in 2.

# Funkcija [alternating_towers] za podano višino vrne število različnih stolpov
# dane višine, ki jih lahko zgradimo z našimi gradniki, kjer se barva gradnikov
# v stolpu izmenjuje (rdeč na modrem, moder na rdečem itd.). Začnemo z gradnikom
# poljubne barve.

# Namig: Uporabi medsebojno rekurzivni pomožni funkciji z ukazom [and].
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# # alternating_towers 10;;
# - : int = 35
#[*----------------------------------------------------------------------------*)

@lru_cache(maxsize=None)
def red(x):
    if x < 0:
        return 0
    elif x <= 1:
        return 1
    else:
        return blue(x - 1) + blue(x - 2)

@lru_cache(maxsize=None)
def blue(x):
    if x < 0:
        return 0
    elif x == 0:
        return 1
    else:
        return red(x - 2) + red(x - 3)

def alternating_towers(x):
    if x == 0:
        return 1
    else:
        return red(x) + blue(x)

#(*----------------------------------------------------------------------------*]
# Na nagradni igri ste zadeli kupon, ki vam omogoča, da v Mercatorju kupite
# poljubne izdelke, katerih skupna masa ne presega [max_w] kilogramov. Napišite
# funkcijo [best_value articles max_w], ki poišče največjo skupno ceno, ki jo
# lahko odnesemo iz trgovine, kjer lahko vsak izdelek vzamemo večkrat, nato pa
# še funkcijo [best_value_uniques articles max_w], kjer lahko vsak izdelek
# vzamemo kvečjemu enkrat.

# Namig: Modul [Array] ponuja funkcije kot so [map], [fold_left], [copy] in
# podobno, kot alternativa uporabi zank.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# # best_value articles 1.;;
# - : float = 10.95
# # best_value_unique articles 1.;;
#- : float = 7.66
#[*----------------------------------------------------------------------------*)

#(* Articles are of form (name, price, weight) *)
#let articles = [|
#	("yoghurt", 0.39, 0.18);
#	("milk", 0.89, 1.03);
#  ("coffee", 2.19, 0.2);
#  ("butter", 1.49, 0.25);
#  ("yeast", 0.22, 0.042);
#  ("eggs", 2.39, 0.69);
#  ("sausage", 3.76, 0.50);
#  ("bread", 2.99, 1.0);
#  ("Nutella", 4.99, 0.75);
#  ("juice", 1.15, 2.0)
#|]

articles = [
    ("yoghurt", 0.39, 0.18),
    ("milk", 0.89, 1.03),
    ("coffee", 2.19, 0.2),
    ("butter", 1.49, 0.25),
    ("yeast", 0.22, 0.042),
    ("eggs", 2.39, 0.69),
    ("sausage", 3.76, 0.50),
    ("bread", 2.99, 1.0),
    ("Nutella", 4.99, 0.75),
    ("juice", 1.15, 2.0)
           ]