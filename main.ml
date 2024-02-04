(* OCaml Program to accepts a list of integers,
emits an error message if the list is not a multiple of 10 in length,
returns or prints a list of integers based on the input list,
but with items at positions which are a multiple of 2 or 3 removed *)

let check_length lst =
  List.length lst mod 10 = 0

let remove_at_multiple_of_2_or_3 lst =
  let rec aux acc index = function
    | [] -> List.rev acc
    | hd :: tl ->
      if (index + 1) mod 2 = 0 || (index + 1) mod 3 = 0 then
        aux acc (index + 1) tl
      else
        aux (hd :: acc) (index + 1) tl
  in
  aux [] 0 lst

let process_list lst =
  if check_length lst then
    let result = remove_at_multiple_of_2_or_3 lst in
    Printf.printf "Processed List: [%s]\n" (String.concat "; " (List.map string_of_int result))
  else
    Printf.printf "Error: The list length is not a multiple of 10.\n"

let () =
  let input_list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20] in
  process_list input_list


  