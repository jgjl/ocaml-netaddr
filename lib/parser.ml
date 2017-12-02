
open Stdint
open Angstrom

type parsed_ipv4 = int * int * int * int

type parsed_ipv6 =
    | ParsedIPv6Complete of int list
    | ParsedIpv6TwoParts of int list * int list

let is_dot =
    function | '.' -> true | _ -> false

let is_colon =
    function | ':' -> true | _ -> false

let is_all_digits = 
    function | '0' .. '9' -> true | _ -> false

let is_all_hexdigits = 
    function 
    | '0' .. '9' -> true 
    | 'a' .. 'f' -> true 
    | 'A' .. 'F' -> true 
    | _ -> false

let is_zero = 
    function | '0' -> true | _ -> false

let is_one = 
    function | '1' -> true | _ -> false

let is_two = 
    function | '2' -> true | _ -> false

let is_five = 
    function | '5' -> true | _ -> false

let is_f = 
    function | 'f' -> true | _ -> false

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
        (satisfy is_all_digits)

let read_byte_3digits_1XX =
    lift3 (fun dig1 dig2 dig3 -> (String.make 1 dig1) ^ (String.make 1 dig2) ^ (String.make 1 dig3))
        (satisfy is_one) 
        (satisfy is_all_digits) 
        (satisfy is_all_digits)

let read_byte_3digits_0XX =
    lift3 (fun dig1 dig2 dig3 -> (String.make 1 dig1) ^ (String.make 1 dig2) ^ (String.make 1 dig3))
        (satisfy is_zero) 
        (satisfy is_all_digits) 
        (satisfy is_all_digits)

let read_byte_2digits_XX =
    lift2 (fun dig1 dig2 -> (String.make 1 dig1) ^ (String.make 1 dig2))
        (satisfy is_all_digits) 
        (satisfy is_all_digits)

let read_byte_1digits_X =
    lift (fun dig1 -> String.make 1 dig1)
        (satisfy is_all_digits)

let testbyte =
    (choice [read_byte_3digits_25X;
            read_byte_3digits_20_4X;
            read_byte_3digits_1XX;
            read_byte_3digits_0XX;
            read_byte_2digits_XX;
            read_byte_1digits_X;
            ]) <* end_of_string


let rec at_most m p =
  (*
    Contributed by seliopou
    https://github.com/inhabitedtype/angstrom/issues/110
  *)
  if m = 0
  then return []
  else
    (lift2 (fun x xs -> x :: xs) p (at_most (m - 1) p))
    <|> return []

let rec at_most_split m pf pr =
  (*
    m = max. count
    pf = first part to match
    pr = rest part to match
  *)
  if m = 0
  then return []
  else
    (lift2 (fun x xs -> x :: xs) 
        pf 
        (at_most_split (m - 1) pf pr))
    <|>
    (lift2 (fun x xs -> x :: xs) 
        pr 
        (at_most_split (m - 2) pr pr))
    <|> return []

let limits n m p =
  (*
    Contributed by seliopou
    https://github.com/inhabitedtype/angstrom/issues/110
  *)
  lift2 (fun xs ys -> xs @ ys)
    (count   n p)
    (at_most m p)

let stringbytes_to_list b1 b2 b3 b4 =
    [int_of_string b1; int_of_string b2; int_of_string b3; int_of_string b4]

let parser_ipv4_part = (lift4 (fun b1 b2 b3 b4 -> int_of_string b1, int_of_string b2, int_of_string b3, int_of_string b4)
    (testbyte <* (skip is_dot))  
    (testbyte <* (skip is_dot)) 
    (testbyte <* (skip is_dot)) 
    testbyte)

let parser_ipv4 = 
  parser_ipv4_part <* end_of_input

let parse_ipv4 ipv4_string =
    match parse_string parser_ipv4 ipv4_string with
    | Result.Ok (b1,b2,b3,b4) -> (string_of_int b1) ^ "." ^ string_of_int b2 ^ "." ^ string_of_int b3 ^ "." ^ string_of_int b4
    | Result.Error message -> message

let read_16bit =
    lift2 (fun f r -> (String.make 1 f) ^ r)
        (satisfy is_all_hexdigits)
        (
        scan_string 0 (fun pos c -> if (is_all_hexdigits c) && pos < 3 then
                                Some (pos + 1)
                            else
                                None) 
        )

let int_of_hex_string s =
    int_of_string ("0x" ^ s)


let merge_ffff_dq m (b1, b2, b3, b4) =
    let second_last = (b1 lsl 8) lor b2 in
    let last = (b3 lsl 8) lor b4 in
    let part2 = [int_of_hex_string m; second_last; last] in
    ParsedIpv6TwoParts ([], part2)

let parser_ipv6_part = 
  (* All 8 16bit fields *)
  lift2 (fun m e -> ParsedIPv6Complete (List.map int_of_hex_string (List.concat [m;[e]])))
      (count 7 (read_16bit <* (skip is_colon)))
      (read_16bit <* end_of_input)
  <|>
  (* (XXXX:)+ (:XXXX)+ *)
  lift2 (fun m e -> ParsedIpv6TwoParts ((List.map int_of_hex_string m), (List.map int_of_hex_string e)) )
      ((limits 1 6 (read_16bit <* (skip is_colon))))
      ((limits 1 6 ((skip is_colon) *> read_16bit)) <* end_of_input)
  <|>
  (* Embedded IPv4 addresses *)
  lift2 merge_ffff_dq
      ((satisfy is_colon) *> (skip is_colon) *> (string_ci "ffff"))
      ((skip is_colon) *> parser_ipv4_part)
  <|>
  (* : (:XXXX)+ *)
  lift2 (fun m e -> ParsedIpv6TwoParts ([], (List.map int_of_hex_string e)) )
      (satisfy is_colon)
      ((many1 ((skip is_colon) *> read_16bit)) <* end_of_input)
  <|>
  (* (XXXX:)+ : *)
  lift2 (fun m e -> ParsedIpv6TwoParts ((List.map int_of_hex_string m), []) )
      ((many1 (read_16bit <* (skip is_colon))))
      ((satisfy is_colon) <* end_of_input)
  <|>
  (* :: *)
  lift (fun m -> ParsedIpv6TwoParts ([], []) )
      ((satisfy is_colon) <* (satisfy is_colon) <* end_of_input)

let parser_ipv6_part_new =
  at_most_split 8 (read_16bit <* (satisfy is_colon)) ((satisfy is_colon) *> read_16bit)

let parser_ipv6 = 
  parser_ipv6_part <* end_of_input

let parse_ipv6 ipv6_string =
  match parse_string parser_ipv6 ipv6_string with
  | Result.Ok (ParsedIPv6Complete result) -> String.concat "-:-" (List.map string_of_int result)
  | Result.Ok ParsedIpv6TwoParts (result1, result2) -> 
      String.concat "-:-" (List.map string_of_int result1) ^ "::" ^
      String.concat "-:-" (List.map string_of_int result2)
  | Result.Error message -> "ERROR: " ^ message


let parser_ipv6_new = 
  parser_ipv6_part_new <* end_of_input

let parse_ipv6_new ipv6_string =
  match parse_string parser_ipv6_new ipv6_string with
  | Result.Ok result -> String.concat "-:-" result
  | Result.Error message -> "ERROR: " ^ message