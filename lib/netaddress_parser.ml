
open Stdint
open Angstrom

let is_dot =
    function | '.' -> true | _ -> false

let is_colon =
    function | ':' -> true | _ -> false

let is_all_hexdigits = 
    function 
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true 
    | _ -> false

let end_of_string = 
    peek_char
    >>= function
        | None | Some '.' -> return ()
        | Some c -> fail ("end_of_string, next char: " ^ (String.make 1 c))

(*
Parse values in {0..2^5} used for prefixes of IPv4 addresses.
*)
type read_5bit_dec_state =
| R5BDS_start
| R5BDS_r_09
| R5BDS_r_02
| R5BDS_end

let read_5bit_dec =
  (*
    Design reading bytes as state machine.
    Accept is implicitly enabled for all states by the default -> None transition.
  *)
  (scan R5BDS_start 
    (fun (state) c -> 
        match state, c with
        | R5BDS_start, '0'        -> Some R5BDS_end
        | R5BDS_start, '1' .. '2' -> Some R5BDS_r_09
        | R5BDS_start, '3'        -> Some R5BDS_r_02
        | R5BDS_r_09,  '0' .. '9' -> Some R5BDS_end
        | R5BDS_r_02,  '0' .. '2' -> Some R5BDS_end
        | _, _ -> None
        )) >>= function 
                | result, R5BDS_start -> fail "Could not read 5bit value"
                | result, _ -> return result 

(*
Parse values in {0..2^7} used for prefixes of IPv6 addresses.
*)
type read_7bit_dec_state =
| R7BDS_start
| R7BDS_1
| R7BDS_1_2
| R7BDS_2_5
| R7BDS_r_09
| R7BDS_end

let read_7bit_dec =
  (*
    Design reading bytes as state machine.
    Accept is implicitly enabled for all states by the default -> None transition.
  *)
  (scan R7BDS_start 
    (fun (state) c -> 
        match state, c with
        | R7BDS_start, '0'        -> Some R7BDS_end
        | R7BDS_start, '1'        -> Some R7BDS_1
        | R7BDS_start, '2' .. '9' -> Some R7BDS_r_09
        | R7BDS_1,     '0' .. '1' -> Some R7BDS_r_09
        | R7BDS_1,     '2'        -> Some R7BDS_1_2
        | R7BDS_1,     '3' .. '9' -> Some R7BDS_end
        | R7BDS_1_2,   '0' .. '8' -> Some R7BDS_end
        | R7BDS_r_09,  '0' .. '9' -> Some R7BDS_end
        | _, _ -> None
        )) >>= function 
                | result, R7BDS_start -> fail "Could not read 7bit value"
                | result, _ -> return result 

(*
Parse values in {0..2^8} used for bytes in the IPv4 dotted quad notation.
*)
type read_byte_dec_state =
| RBDS_start
| RBDS_01
| RBDS_2
| RBDS_39
| RBDS_2_5
| RBDS_r_09
| RBDS_end

let read_byte_dec =
  (*
    Design reading bytes as state machine.
    Accept is implicitly enabled for all states by the default -> None transition.
  *)
  (scan RBDS_start 
    (fun (state) c -> 
        match state, c with
        | RBDS_start, '0' .. '1' -> Some RBDS_01
        | RBDS_start, '2'        -> Some RBDS_2
        | RBDS_start, '3' .. '9' -> Some RBDS_39
        | RBDS_01,    '0' .. '9' -> Some RBDS_r_09
        | RBDS_r_09,  '0' .. '9' -> Some RBDS_end
        | RBDS_39,    '0' .. '9' -> Some RBDS_end
        | RBDS_2,     '0' .. '4' -> Some RBDS_r_09
        | RBDS_2,     '5'        -> Some RBDS_2_5
        | RBDS_2,     '6' .. '9' -> Some RBDS_end
        | RBDS_2_5,   '0' .. '5' -> Some RBDS_end
        | _, _ -> None
        )) >>= function 
                | result, RBDS_start -> fail "Could not read byte value"
                | result, _ -> return result 

(*
Parse values in {0..2^8} used for bytes in the EUI coloned hexa notation.
*)
type read_byte_hex_state =
| RBHS_start
| RBHS_first
| RBHS_end

let read_byte_hex =
  (scan RBHS_start 
    (fun (state) c -> 
        match state, c with
        | RBHS_start, '0' .. 'f' -> Some RBHS_first
        | RBHS_first, '0' .. 'f' -> Some RBHS_end
        | _, _ -> None
        )) >>= function 
                | result, RBHS_start -> fail "Could not read byte value"
                | result, _ -> return result 

let read_16bit_hex =
    lift2 (fun f r -> (String.make 1 f) ^ r)
        (satisfy is_all_hexdigits)
        (scan_string 0 (fun pos c -> if (is_all_hexdigits c) && pos < 3 then
                              Some (pos + 1)
                            else
                              None))

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

let limits n m p =
  (*
    Contributed by seliopou
    https://github.com/inhabitedtype/angstrom/issues/110
  *)
  lift2 (fun xs ys -> xs @ ys)
    (count   n p)
    (at_most m p)

let rec at_most_split c m pf pr s f =
  (*
    m = max. count
    pf = first part to match
    pr = rest part to match
    s = split part
    f = split part for the first read field
  *)
  if c = m-1 then
      (lift (fun x -> ([x], [])) s)
  else 
    (* Parse (XXXX:)* parts *)
    (lift2 (fun x1 (xs1, xs2) -> (x1 :: xs1, xs2)) pf (at_most_split (c+1) m pf pr s f))
    <|>
    (* Parse (:XXXX) parts, special case handling for addresses starting with "::" *)
    (
      if c = 0 then (
        lift2 (fun _ xs2 -> ([], xs2)) f (limits 1 6 pr)
        <|>
        (lift2 (fun _ _ -> ([], [])) f f)
        )
      else (
        (*(lift2 (fun x2 xs2 -> ([], x2::xs2)) s (limits 1 (m-c-1) pr))*)
        (lift (fun xs -> ([], xs)) (limits 1 (m-c-1) pr))
        <|>
        (lift (fun _ -> ([], [])) f)
      )
    )


module Eui48 = struct
  type parsed_value = int * int * int * int * int * int
  type parsed_value_prefix = int

  let min_str_length_address = 11

  let max_str_length_address = 17

  let stringbytes_to_list b1 b2 b3 b4 b5 b6 =
      [int_of_string b1; int_of_string b2; int_of_string b3; int_of_string b4]

  let parser_eui48_part =
    lift4 (fun b1 b2 b3 b4 b5 b6 -> 
               (int_of_string b1, 
               int_of_string b2, 
               int_of_string b3, 
               int_of_string b4, 
               int_of_string b5, 
               int_of_string b6))
      (read_byte_hex <* (skip is_colon))
      (read_byte_hex <* (skip is_colon))
      (read_byte_hex <* (skip is_colon))
      (read_byte_hex <* (skip is_colon))
    <*> 
    (read_byte_hex <* (skip is_colon))
    <*>
    read_byte_hex

  let parser_address = 
    parser_eui48_part <* end_of_input

  let parse_eui48 eui48_string =
      match parse_string parser_address eui48_string with
      | Result.Ok (b1,b2,b3,b4,b5,b6) -> 
                    string_of_int b1 ^ ":" ^ 
                    string_of_int b2 ^ ":" ^ 
                    string_of_int b3 ^ ":" ^ 
                    string_of_int b4 ^ ":" ^ 
                    string_of_int b5 ^ ":" ^ 
                    string_of_int b6
      | Result.Error message -> message
end


module IPv4 = struct
  type parsed_value = int * int * int * int
  type parsed_value_prefix = int

  let min_str_length_address = 7
  let min_str_length_range = 15
  let min_str_length_network = 10

  let max_str_length_address = 15
  let max_str_length_range = 31
  let max_str_length_network = 18

  let stringbytes_to_list b1 b2 b3 b4 =
      [int_of_string b1; int_of_string b2; int_of_string b3; int_of_string b4]

  let parser_ipv4_part =
    (lift4
      (fun b1 b2 b3 b4 -> int_of_string b1, int_of_string b2, int_of_string b3, int_of_string b4)
      (read_byte_dec <* (skip is_dot))
      (read_byte_dec <* (skip is_dot))
      (read_byte_dec <* (skip is_dot))
      read_byte_dec)

  let parser_address = 
    parser_ipv4_part <* end_of_input

  let parser_range =
    lift2 (fun first_value last_value -> (first_value, last_value))
    (parser_ipv4_part <* char '-')
    (parser_ipv4_part <* end_of_input)

  let parser_network =
    lift2 (fun network_value prefix_len -> (network_value, int_of_string prefix_len))
    (parser_ipv4_part <* char '/')
    (read_5bit_dec <* end_of_input)

  let parse_ipv4 ipv4_string =
      match parse_string parser_address ipv4_string with
      | Result.Ok (b1,b2,b3,b4) -> (string_of_int b1) ^ "." ^ string_of_int b2 ^ "." ^ string_of_int b3 ^ "." ^ string_of_int b4
      | Result.Error message -> message
end

module IPv6 = struct
  type parsed_value = int list * int list
  type parsed_value_prefix = int

  let min_str_length_address = 2
  let min_str_length_range = 5
  let min_str_length_network = 4

  let max_str_length_address = 39
  let max_str_length_range = 79
  let max_str_length_network = 43

  let int_of_hex_string s =
      int_of_string ("0x" ^ s)

  let parser_value_part =
    at_most_split 0 8
      (read_16bit_hex <* (satisfy is_colon))
      ((satisfy is_colon) *> read_16bit_hex)
      read_16bit_hex
      (*
      (read_16bit <|> (peek_string 1 >>= function | ":" -> return "" | s -> fail s ))
      *)
      (string ":")

  let parser_address =
    (parser_value_part <* end_of_input)
    >>|
    (fun (part1, part2) -> (List.map int_of_hex_string part1, List.map int_of_hex_string part2))

  let parser_range =
    lift2 (fun (s_p1, s_p2) (e_p1, e_p2) ->
            (List.map int_of_hex_string s_p1, List.map int_of_hex_string s_p2),
            (List.map int_of_hex_string e_p1, List.map int_of_hex_string e_p2))
    (parser_value_part <* char '-')
    (parser_value_part <* end_of_input)

  let parser_network =
    lift2 (fun (s_p1, s_p2) prefix_len ->
            (List.map int_of_hex_string s_p1, List.map int_of_hex_string s_p2), (int_of_string prefix_len))
    (parser_value_part <* char '/')
    (read_7bit_dec <* end_of_input)
end
