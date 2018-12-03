let f1 list = 
  let rec f1' acc = function
    | [] -> acc
    | x :: xs -> 
      let new_acc = x + acc in
      f1' new_acc xs
  in
  f1' 0 list

let f2 list = 
  let rec f2' acc = function
    | [] -> true
    | x :: xs -> 
      if x >= acc then
        let new_acc = x in
        f1' new_acc xs
      else
        false
  and first = function
    | [] -> 0
    | y :: _ -> y
  in
  f1' (first list) list

let f31 list =
  let rec f31' acc = function
    | [] -> acc
    | x :: xs -> 
      let rec insert y list' =
        if (f2 (y :: list')) then
          y :: list'
        else
          (let first = function
            | [] -> []
            | z :: _ -> [z]
          and rest = function
            | [] -> []
            | _ :: zs -> zs
          in
          first list' @ (insert y (rest list')
          )
      in
      let new_acc = insert x acc in
      f31' new_acc xs
  in
  f31' [] list

let f32 x list = f31 (x :: list)

let f4 cmp list =
  let rec f4' acc = function
    | [] -> acc
    | x :: xs -> 
      let rec insert y list' =
        let first = (function
          | [] -> []
          | z :: _ -> [z])
        and rest = (function
          | [] -> []
          | _ :: zs -> zs)
        in
        if (cmp y (first list')) then
          y :: list'
        else
          first list' @ (insert y (rest list'))
      in
      let new_acc = insert x acc in
      f4' new_acc xs
  in
  f4' [] list

type priority =
  | Top
  | Group of int

type status =
  | Staff
  | Passenger of priority

type flyer = { status : status ; name : string }

let flyers = [ {status = Staff; name = "Quinn"}
             ; {status = Passenger (Group 0); name = "Xiao"}
             ; {status = Passenger Top; name = "Jaina"}
             ; {status = Passenger (Group 1000); name = "Aleks"}
             ; {status = Passenger (Group 1000); name = "Robin"}
             ; {status = Staff; name = "Alan"}
             ]

(* -------- 6 -------- *)

(* -------- 7 -------- *)