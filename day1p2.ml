(* oc str.cma day2.ml *)
(* #load "str.cma";; *)
open List
open String
open Str

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

let tab = ["zero","0"; "one","1"; "two","2"; "three","3"; "four","4";
           "five","5"; "six","6"; "seven","7"; "eight","8"; "nine","9"] ;;
let prepare s = List.fold_left
  (fun acc (f, t) -> Str.global_replace (Str.regexp f) (f ^ t ^ f) acc) s tab ;;


let char2str c = String.of_seq @@ List.to_seq [c]
let get_num s = 
  let d = List.filter digit @@ explode @@ prepare s in
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
      (* List.map (print_endline << prepare) ln *)

