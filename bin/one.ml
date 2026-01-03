let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t
;;

let print_result = function
  | None -> print_endline "None"
  | Some l -> Printf.printf "Some \"%s\"\n" l
;;

let () =
  print_result (last [ "a"; "b"; "c"; "d" ]);
  print_result (last [])
;;
