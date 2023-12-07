open List
open String

let (<<) f g x = f (g x) ;;
exception Exc of string

let rec last l = match l with
    | [] -> raise (Exc "last: empty list")
    | [h] -> h
    | (_::t) -> last t 


let explode s = s |> String.to_seq |> List.of_seq ;;

let digit = function
  | '0' .. '9' -> true 
  | _ -> false 
  ;;

let char2str c = String.of_seq @@ List.to_seq [c]
let get_num s = 
  let d = List.filter digit @@ explode s in
    int_of_string @@ String.concat "" @@ List.map char2str [List.hd d; last d]
    ;;

let rec sum l = match l with 
  | [] -> 0
  | h::t -> h + sum t ;;

let lines = ref [] in
  let ln = (try
    while true; do
      lines := input_line stdin :: !lines
    done;
    !lines
  with End_of_file ->
    List.rev !lines) in
      print_int @@ sum @@ List.map get_num ln
