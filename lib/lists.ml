(* 01 - Tail of a List *)
let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t
;;

let%test _ = last [ "a"; "b"; "c"; "d" ] = Some "d"
let%test _ = last [] = None

(* 02 - Last Two Elements of a List *)
let rec last_two = function
  | [] -> None
  | [ a; b ] -> Some (a, b)
  | _ :: t -> last_two t
;;

let%test _ = last_two [ "a"; "b"; "c"; "d" ] = Some ("c", "d")
let%test _ = last_two [ "a" ] = None

(* 03 - N'th Element of a List *)
let rec at n = function
  | [] -> None
  | h :: t -> if n = 0 then Some h else at (n - 1) t
;;

let%test _ = at 2 [ "a"; "b"; "c"; "d"; "e" ] = Some "c"
let%test _ = at 2 [ "a" ] = None

(* 04 - Lenght of a List *)
let length lst =
  let rec loop count = function
    | [] -> count
    | _ :: t -> loop (count + 1) t
  in
  loop 0 lst
;;

let%test _ = length [ "a"; "b"; "c" ] = 3
let%test _ = length [] = 0

(* 05 - Reverse a List *)
let rev lst =
  let rec loop acc = function
    | [] -> acc
    | h :: t -> loop (h :: acc) t
  in
  loop [] lst
;;

let%test _ = rev [ "a"; "b"; "c" ] = [ "c"; "b"; "a" ]

(* 17 - Split a List Into Two Parts; The Length of the First Part Is Given *)
let split list n =
  if n < 0 || n >= List.length list
  then list, []
  else (
    let rec aux k acc = function
      | [] -> [], []
      | h :: t -> if k = 1 then List.rev (h :: acc), t else aux (k - 1) (h :: acc) t
    in
    aux n [] list)
;;

let%test _ =
  split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
  = ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ])
;;

let%test _ = split [ "a"; "b"; "c"; "d" ] 5 = ([ "a"; "b"; "c"; "d" ], [])

(* 18 - Extract a Slice From a List *)
let slice list s e =
  if s < 0 || e >= List.length list
  then []
  else (
    let rec aux n acc = function
      | [] -> []
      | h :: t ->
        if n < s
        then aux (n + 1) acc t
        else if n >= s && n <= e
        then aux (n + 1) (h :: acc) t
        else List.rev acc
    in
    aux 0 [] list)
;;

let%test _ =
  slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6
  = [ "c"; "d"; "e"; "f"; "g" ]
;;

let%test _ = slice [ "a"; "b"; "c" ] 2 4 = []

(* 19 - Rotate a List N Places to the Left *)
let rotate lst n =
  let rec loop lst k acc =
    match k, lst with
    | _, [] -> if List.is_empty acc then [] else List.rev acc
    | 0, l -> l @ List.rev acc
    | _, h :: t -> loop t (k - 1) (h :: acc)
  in
  loop lst n []
;;

let%test _ =
  rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3
  = [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ]
;;

let%test _ = rotate [] 1 = []
let%test _ = rotate [ "a"; "b"; "c" ] 3 = [ "a"; "b"; "c" ]
