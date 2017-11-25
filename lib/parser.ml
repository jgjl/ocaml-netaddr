
open Stdint
open Angstrom

let is_dot =
    function | '.' -> true | _ -> false

let is_alldigits = 
    function | '0' .. '9' -> true | _ -> false

let is_zero = 
    function | '0' -> true | _ -> false

let is_one = 
    function | '1' -> true | _ -> false

let is_two = 
    function | '2' -> true | _ -> false

let is_five = 
    function | '5' -> true | _ -> false

let is_two2four = 
    function | '0' .. '4' -> true | _ -> false

let is_bytedigit_five = 
    function | '0' .. '5' -> true | _ -> false

let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

let byte =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| Uint32.of_string

let end_of_string = 
    peek_char
    >>= function
        | None | Some '.' -> return ()
        | Some c-> fail ("end_of_string, next char: " ^ (String.make 1 c))

let read_block =
    take_till is_dot 

let read_byte_3digits_25X =
    lift3 (fun dig1 dig2 dig3 -> (String.make 1 dig1) ^ (String.make 1 dig2) ^ (String.make 1 dig3))
        (satisfy is_two) 
        (satisfy is_five) 
        (satisfy is_bytedigit_five)

let read_byte_3digits_20_4X =
    lift3 (fun dig1 dig2 dig3 -> (String.make 1 dig1) ^ (String.make 1 dig2) ^ (String.make 1 dig3))
        (satisfy is_two) 
        (satisfy is_two2four) 
        (satisfy is_alldigits)

let read_byte_3digits_1XX =
    lift3 (fun dig1 dig2 dig3 -> (String.make 1 dig1) ^ (String.make 1 dig2) ^ (String.make 1 dig3))
        (satisfy is_one) 
        (satisfy is_alldigits) 
        (satisfy is_alldigits)

let read_byte_3digits_0XX =
    lift3 (fun dig1 dig2 dig3 -> (String.make 1 dig1) ^ (String.make 1 dig2) ^ (String.make 1 dig3))
        (satisfy is_zero) 
        (satisfy is_alldigits) 
        (satisfy is_alldigits)

let read_byte_2digits_XX =
    lift2 (fun dig1 dig2 -> (String.make 1 dig1) ^ (String.make 1 dig2))
        (satisfy is_alldigits) 
        (satisfy is_alldigits)

let read_byte_1digits_X =
    lift (fun dig1 -> String.make 1 dig1)
        (satisfy is_alldigits)

let testbyte =
    (choice [read_byte_3digits_25X;
            read_byte_3digits_20_4X;
            read_byte_3digits_1XX;
            read_byte_3digits_0XX;
            read_byte_2digits_XX;
            read_byte_1digits_X;
            ]) <* end_of_string

let stringbytes_to_list b1 b2 b3 b4 =
    [int_of_string b1; int_of_string b2; int_of_string b3; int_of_string b4]

let parser_ipv4 = (lift4 stringbytes_to_list
    (testbyte <* (skip is_dot))  
    (testbyte <* (skip is_dot)) 
    (testbyte <* (skip is_dot)) 
    (testbyte <* end_of_input)) 

let parse_ipv4 ipv4_string =
    match parse_string parser_ipv4 ipv4_string with
    | Result.Ok result -> String.concat "." (List.map string_of_int result) 
    | Result.Error message -> message

(*
let parser_ipv6 = 
    lift (fun x -> print_string x; [1])
    (*
    (lift4 bytes_to_uint128
    (testbyte <* (skip is_dot))  
    (testbyte <* (skip is_dot)) 
    (testbyte <* (skip is_dot)) 
    (testbyte <* end_of_input)) 
    *)

let parse_ipv6 ipv6_string =
    match parse_string parser_ipv6 ipv6_string with
    | Result.Ok result -> result (*String.concat ":" (List.map string_of_int result)*)
    | Result.Error message -> message
*)