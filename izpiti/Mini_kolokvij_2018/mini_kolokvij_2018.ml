let f1 xs = 
  let rec f1' acc = function
    | [] -> acc
    | y :: ys -> 
      let new_acc = y + acc in
      f1' new_acc ys
  in
  f1' 0 xs

let f2 = function
  | [] -> true
  | x :: xs ->
    let rec f2' acc = function
      | [] -> true
      | y :: ys -> 
        if y >= acc then
          let new_acc = y in
          f2' new_acc ys
        else
          false
    in
    f2' x (x :: xs)

let f31 x xs =
  let rec f31' acc = function
    | [] -> List.rev (x :: acc)
    | y :: ys ->
      if x < y then
        (List.rev acc) @ (x :: y :: ys)
      else  
        let new_acc = y :: acc in
        f31' new_acc ys
  in
  f31' [] xs

let f32 xs =
  let rec f32' acc = function
    | [] -> acc
    | y :: ys -> 
      let new_acc = f31 y acc in
      f32' new_acc ys
  in
  f32' [] xs

let f41 cmp x xs =
  let rec f41' acc = function
    | [] -> List.rev (x :: acc)
    | y :: ys ->
      if (cmp x y) then
        (List.rev acc) @ (x :: y :: ys)
      else  
        let new_acc = y :: acc in
        f41' new_acc ys
  in
  f41' [] xs
    
let f42 cmp xs =
  let rec f42' acc = function
    | [] -> acc
    | y :: ys -> 
      let new_acc = (f41 cmp y acc) in
      f42' new_acc ys
  in
  f42' [] xs

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