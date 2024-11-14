(*
 * SPDX-FileCopyrightText: 2023-2024 Eli Array Minkoff
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

(* Based on the language docs - see https://ocaml.org/docs/tour-of-ocaml *)
let rec range lo hi step =
    if step <= 0 then
        []
    else if lo >= hi then
        []
    else
        lo :: range (lo + step) hi step
;;

let color_cell = fun n -> print_string ("\x1b[48;5;" ^ (string_of_int n) ^ "m  ") ;;

let cube_row_part = fun n -> (List.iter color_cell (range (n) (n + 6) 1));;

let cube_row = fun n ->
    cube_row_part n ;
    print_string "\x1b[0m  " ;
    cube_row_part (n + 36) ;
    print_string "\x1b[0m  " ;
    cube_row_part (n + 72) ;
    print_string "\x1b[0m\n" ;;

(* Print the first 16 colors - these vary by terminal configuration *)
print_string "\n";;
List.iter color_cell (range 0 16 1) ;;
print_string "\x1b[0m\n\n" ;;

(* Print the 6 sides of the color cube - these are more standardized,
 * but the order is a bit odd, thus the need for the above trickery *)
List.iter cube_row (range 16 52 6) ;;
print_string "\n" ;;
List.iter cube_row (range 124 160 6) ;;
print_string "\n" ;;

(* Finally, the 24 grays *)
List.iter color_cell (range 232 256 1) ;;
print_string "\x1b[0m\n\n" ;;
