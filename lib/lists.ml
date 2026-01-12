(* 01 *)
let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t
;;

let%test _ = last [ "a"; "b"; "c"; "d" ] = Some "d"
let%test _ = last [] = None

(* 02 *)
let rec last_two = function
  | [] -> None
  | [ a; b ] -> Some (a, b)
  | _ :: t -> last_two t
;;

let%test _ = last_two [ "a"; "b"; "c"; "d" ] = Some ("c", "d")
let%test _ = last_two [ "a" ] = None

(* 03 *)
let rec at n = function
  | [] -> None
  | h :: t -> if n = 0 then Some h else at (n - 1) t
;;

let%test _ = at 2 [ "a"; "b"; "c"; "d"; "e" ] = Some "c"
let%test _ = at 2 [ "a" ] = None

(* 04 *)
let length lst =
  let rec loop count = function
    | [] -> count
    | _ :: t -> loop (count + 1) t
  in
  loop 0 lst
;;

let%test _ = length [ "a"; "b"; "c" ] = 3
let%test _ = length [] = 0

(* 05 *)
let rev lst =
  let rec loop acc = function
    | [] -> acc
    | h :: t -> loop (h :: acc) t
  in
  loop [] lst
;;

let%test _ = rev [ "a"; "b"; "c" ] = [ "c"; "b"; "a" ]

(* 17 *)
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

(* 18 *)
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
