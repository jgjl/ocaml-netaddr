
open Stdint
open Angstrom

let is_dot =
    function | '.' -> true | _ -> false

let is_alldigits = 
    function | '0' .. '9' -> true | _ -> false

let is_bytedigit_two = 
    function | '0' .. '2' -> true | _ -> false

let is_bytedigit_five = 
    function | '0' .. '5' -> true | _ -> false

let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

let byte =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| Uint32.of_string

let read_block =
    take_till is_dot 

let read_byte_3digits =
    scan 

let bytes_to_value b1 b2 b3 b4 =
    Uint32.(logor b4 (logor (shift_left b3 8) (logor (shift_left b2 16) (shift_left b1 24))))

let read_dq = (lift4 bytes_to_value
    (byte <* (skip is_dot))  
    (byte <* (skip is_dot)) 
    (byte <* (skip is_dot)) 
    (byte <* end_of_input)) 

let parse_ipv4 ipv4_string =
    match parse_string read_dq ipv4_string with
    | Result.Ok result -> Uint32.to_string result
    | Result.Error message -> message

(*
 parse_string ((count 4 ((integer) <* ((skip is_dot) <|> end_of_input))) <* end_of_input) "123.2343.454.35";;
 parse_string (lift4 (fun b1 b2 b3 b4 -> (b1,b2,b3,b4)) 
    (integer <* (skip is_dot))  
    (integer <* (skip is_dot)) 
    (integer <* (skip is_dot)) 
    (integer <* end_of_input)) 
    "1.2.3.4";;
*)

(*
let read_byte_2digits =

let read_byte_1digit =
*)
(*
let parse_address address_string =
(*parse_string integer address_string*)
parse_string 
    (read_block >>= (scan_state "" 
    (fun s c -> if (is_alldigits c) then
                    Some (s ^ (String.make 1 c))
                else
                    None))) address_string;;
                    *)