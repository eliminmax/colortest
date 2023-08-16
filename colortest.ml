(* Python-style range from StackOverflow user Anentropic
   https://stackoverflow.com/a/70613105 *)
let range ?(from=0) until ?(step=1) =
  let cmp = match step with
    | i when i < 0 -> (>)
    | i when i > 0 -> (<)
    | _ -> raise (Invalid_argument "step must not be zero")
  in
  Seq.unfold (function
        i when cmp i until -> Some (i, i + step) | _ -> None
    ) from
;;

let colorcell = fun n -> print_string ("\x1b[48;5;" ^ (string_of_int n) ^ "m  ") ;;

(* Print the first 16 colors - these vary by terminal configuration *)
print_string "\n";;
List.iter colorcell (List.of_seq @@ range 16) ;;
print_string "\x1b[0m\n\n" ;;

(* Print the 6 sides of the color cube - these are more standardized,
 * but the order is a bit odd, thus the need for this trickery *)
let row_a = fun n -> (List.iter colorcell (List.of_seq @@ range ~from:(n) (n + 6))) ;;
let row_b = fun n -> (List.iter colorcell (List.of_seq @@ range ~from:(n + 36) (n + 42))) ;;
let row_c = fun n -> (List.iter colorcell (List.of_seq @@ range ~from:(n + 72) (n + 78))) ;;
let part_2_row = fun n -> 
    row_a n ;
    print_string "\x1b[0m  " ;
    row_b n ;
    print_string "\x1b[0m  " ;
    row_c n ;
    print_string "\x1b[0m\n" ;;
List.iter part_2_row (List.of_seq @@ range ~from:16 52 ~step:6) ;;
print_string "\n" ;;
List.iter part_2_row (List.of_seq @@ range ~from:124 160 ~step:6) ;;
print_string "\n" ;;

(* Finally, the 24 grays *)
List.iter colorcell (List.of_seq @@ range ~from:232 256) ;;
print_string "\x1b[0m\n\n" ;;
