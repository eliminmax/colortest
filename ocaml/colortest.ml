let colorcell = fun n -> print_string ("\x1b[48;5;" ^ (string_of_int n) ^ "m  ") ;;

(* Based on the language docs - see https://ocaml.org/docs/tour-of-ocaml *)
let rec range lo hi step =
    if step <= 0 then
        []
    else if lo >= hi then
        []
    else
        lo :: range (lo + step) hi step
;;

(* Print the first 16 colors - these vary by terminal configuration *)
print_string "\n";;
List.iter colorcell (range 0 16 1) ;;
print_string "\x1b[0m\n\n" ;;

(* Print the 6 sides of the color cube - these are more standardized,
 * but the order is a bit odd, thus the need for this trickery *)
let row_a = fun n -> (List.iter colorcell (range n (n + 6) 1)) ;;
let row_b = fun n -> (List.iter colorcell (range (n + 36) (n + 42) 1)) ;;
let row_c = fun n -> (List.iter colorcell (range (n + 72) (n + 78) 1)) ;;
let part_2_row = fun n -> 
    row_a n ;
    print_string "\x1b[0m  " ;
    row_b n ;
    print_string "\x1b[0m  " ;
    row_c n ;
    print_string "\x1b[0m\n" ;;
List.iter part_2_row (range 16 52 6) ;;
print_string "\n" ;;
List.iter part_2_row (range 124 160 6) ;;
print_string "\n" ;;

(* Finally, the 24 grays *)
List.iter colorcell (range 232 256 1) ;;
print_string "\x1b[0m\n\n" ;;
