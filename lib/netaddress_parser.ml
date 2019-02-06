
(* open Stdint *)
open Angstrom

let rec at_most m p =
  (*
    Contributed by seliopou
    https://github.com/inhabitedtype/angstrom/issues/110
  *)
  if m = 0 then 
    return []
  else
    (lift2 (fun x xs -> x :: xs) p (at_most (m - 1) p))
    (* <|> return [] *)
;;

let limits n m p =
  (*
    Contributed by seliopou
    https://github.com/inhabitedtype/angstrom/issues/110
  *)
  lift2 (fun xs ys -> xs @ ys)
    (count   n p)
    (at_most m p)
;;

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
;;

let is_dot =
    function | '.' -> true | _ -> false
;;

let is_colon =
    function | ':' -> true | _ -> false
;;

let is_all_hexdigits = 
    function 
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true 
    | _ -> false
;;

(* let end_of_string = 
    peek_char
    >>= function
        | None | Some '.' -> return ()
        | Some c -> fail ("end_of_string, next char: " ^ (String.make 1 c)) *)
;;

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
        | R5BDS_start, '4' .. '9' -> Some R5BDS_end
        | R5BDS_r_09,  '0' .. '9' -> Some R5BDS_end
        | R5BDS_r_02,  '0' .. '2' -> Some R5BDS_end
        | _, _ -> None
        )) >>= function 
                | _, R5BDS_start -> fail "Could not read 5bit value"
                | result, _ -> return result 
;;

(*
Parse values in {0..2^7} used for prefixes of IPv6 addresses.
*)
type read_7bit_dec_state =
| R7BDS_start
| R7BDS_1
| R7BDS_1_2
(* | R7BDS_2_5 *)
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
                | _, R7BDS_start -> fail "Could not read 7bit value"
                | result, _ -> return result 
;;

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
                | _, RBDS_start -> fail "Could not read byte value"
                | result, _ -> return result 
;;

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
                | _, RBHS_start -> fail "Could not read byte value"
                | result, _ -> return result 
;;

(*
Parse values in {0..2^16} used for bytes in the IPv6 coloned hexa notation.
*)
let read_16bit_hex_1 =
    lift2 (fun f r -> (String.make 1 f) ^ r)
        (satisfy is_all_hexdigits)
        (scan_string 0 (fun pos c -> if (is_all_hexdigits c) && pos < 3 then
                              Some (pos + 1)
                            else
                              None))
;;

let read_16bit_hex_2 = 
    (* lift2 (fun f r -> ) *)
    (scan_string 4 
        (fun state c -> 
            match state, c with
            | 4, '0' .. '9' | 4, 'a' .. 'f' | 4, 'A' .. 'F' -> Some 3
            | 3, '0' .. '9' | 3, 'a' .. 'f' | 3, 'A' .. 'F' -> Some 2
            | 2, '0' .. '9' | 2, 'a' .. 'f' | 2, 'A' .. 'F' -> Some 1
            | 1, '0' .. '9' | 1, 'a' .. 'f' | 1, 'A' .. 'F' -> Some 0
            | _, _ -> None
            )
        ) (* >>=
            function 
            | "" -> fail "Empty"
            | v -> return v *)
;;

let read_16bit_hex_4 = 
    (scan_string 4 
        (fun n c -> 
            if n < 1 then
                None
            else 
                match c with
                | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> Some (n - 1)
                | _ -> None
            )
    )
;;

let read_16bit_hex_3 = 
  lift (fun cl -> List.fold_left (fun r c -> (r lsl 4) + (int_of_char c)) 0 cl) (at_most 4 (satisfy is_all_hexdigits))
;;

let read_16bit_hex = read_16bit_hex_2

module Eui48 = struct
    type parsed_value = int * int * int * int * int * int
    type parsed_value_prefix = int

    let min_str_length_address = 11

    let max_str_length_address = 17

    (* let stringbytes_to_list b1 b2 b3 b4 b5 b6 =
        [int_of_string b1; int_of_string b2; int_of_string b3; int_of_string b4; int_of_string b5; int_of_string b6]
    ;; *)

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
    ;;

    let parser_address = 
        parser_eui48_part <* end_of_input
    ;;

    (* let parse_eui48 eui48_string =
        match parse_string parser_address eui48_string with
        | Result.Ok (b1,b2,b3,b4,b5,b6) -> 
                        string_of_int b1 ^ ":" ^ 
                        string_of_int b2 ^ ":" ^ 
                        string_of_int b3 ^ ":" ^ 
                        string_of_int b4 ^ ":" ^ 
                        string_of_int b5 ^ ":" ^ 
                        string_of_int b6
        | Result.Error message -> message
    ;; *)
end


module IPv4 = struct
    type parsed_value = int * int * int * int
    type parsed_value_prefix = int

    let min_str_length_address = 7
    let min_str_length_range = 15
    let min_str_length_network = 9

    let max_str_length_address = 15
    let max_str_length_range = 31
    let max_str_length_network = 18

    (* let stringbytes_to_list b1 b2 b3 b4 =
        [int_of_string b1; int_of_string b2; int_of_string b3; int_of_string b4]
    ;; *)

    let parser_ipv4_part =
        (lift4
        (fun b1 b2 b3 b4 -> int_of_string b1, int_of_string b2, int_of_string b3, int_of_string b4)
        (read_byte_dec <* (skip is_dot))
        (read_byte_dec <* (skip is_dot))
        (read_byte_dec <* (skip is_dot))
        read_byte_dec)
    ;;

    let parser_ipv4_part_2 a_make_fun =
        (lift4
        a_make_fun
        (read_byte_dec <* (skip is_dot))
        (read_byte_dec <* (skip is_dot))
        (read_byte_dec <* (skip is_dot))
        read_byte_dec)
    ;;

    let parser_address = 
        parser_ipv4_part <* end_of_input
    ;;

    let parser_address_2 a_make_fun = 
        (parser_ipv4_part_2 a_make_fun) <* end_of_input
    ;;

    let parser_range =
        lift2 (fun first_value last_value -> (first_value, last_value))
        (parser_ipv4_part <* char '-')
        (parser_ipv4_part <* end_of_input)
    ;;

    let parser_range_2 a_make_fun r_make_fun =
        lift2 r_make_fun
        ((parser_ipv4_part_2 a_make_fun) <* char '-')
        ((parser_ipv4_part_2 a_make_fun) <* end_of_input)
    ;;

    let parser_network =
        lift2 (fun network_value prefix_len -> (network_value, int_of_string prefix_len))
        (parser_ipv4_part <* char '/')
        (read_5bit_dec <* end_of_input)

    let parser_network_2 a_make_fun n_make_fun =
        lift2 n_make_fun
        ((parser_ipv4_part_2 a_make_fun) <* char '/')
        (read_5bit_dec <* end_of_input)
    ;;

    (* let parse_ipv4 ipv4_string =
        match parse_string parser_address ipv4_string with
        | Result.Ok (b1,b2,b3,b4) -> (string_of_int b1) ^ "." ^ string_of_int b2 ^ "." ^ string_of_int b3 ^ "." ^ string_of_int b4
        | Result.Error message -> message *)
    ;;
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
    ;;

    let parser_value_part_1 =
        at_most_split 0 8
        (read_16bit_hex <* (satisfy is_colon))
        ((satisfy is_colon) *> read_16bit_hex)
        read_16bit_hex
        (string ":")
    ;;


    type ps =
    | PS_Start
    | PS_bdc of int
    | PS_adc of int
    | PS_End

    let parser_value_part_2 =
        print_endline "fun called";
        let rec parse state (bdc, adc) =
            match state with
            | PS_Start -> 
            begin
                print_endline "PS_Start";
                read_16bit_hex_2 <* (satisfy is_colon) >>=
                    function 
                    | "" -> fail "Start: Empty"
                    | v -> parse (PS_bdc 1) ([v], adc)
            end
            | PS_bdc n when n < 7 -> 
            begin
                Printf.printf "PS_bdc %d < 7\n" n;
                read_16bit_hex_2 <* (satisfy is_colon) >>=
                    function 
                    | "" -> parse (PS_adc (n + 1)) (bdc, adc)
                    | v -> parse (PS_bdc (n + 1)) (v :: bdc, adc)
            end
            | PS_bdc n when n = 7 -> 
            begin
                Printf.printf "PS_bdc %d = 7\n" n;
                read_16bit_hex_2 >>=
                    function 
                    | "" -> fail "Bdc: Empty"
                    | v -> parse PS_End (v :: bdc, adc)
            end
            | PS_adc n when n < 7 -> 
            begin
                Printf.printf "PS_adc %d < 6\n" n;
                (satisfy is_colon) *> read_16bit_hex_2 >>=
                    function 
                    | "" -> fail "Adc: Empty"
                    | v -> parse (PS_adc (n + 1)) (bdc, v :: adc)
            end
            | PS_adc _ | PS_bdc _ -> fail "Adc: wrong char"
            | PS_End _ -> return (List.rev bdc, List.rev adc)
            in
        parse PS_Start ([], [])
                
    type parse_state = 
    | Start
    | Before_dc of int
    | After_dc of int * int
    | End 


    let parser_value_part_3 =
        let rec parse state (bdc, adc) =
            match state with
            | Start -> 
            begin
                ((read_16bit_hex <* (satisfy is_colon)) >>=
                    (fun v ->
                        parse (Before_dc 1) (v :: bdc, adc)))
                <|> ((satisfy is_colon) >>=
                    (fun _ ->
                        parse (After_dc (0, 0)) (bdc, adc)))
            end 
            | Before_dc bn when bn < 7 ->
            begin
                ((read_16bit_hex <* (satisfy is_colon)) >>=
                    (fun v ->
                        parse (Before_dc (bn + 1)) (v :: bdc, adc)))
                <|> ((satisfy is_colon) *> read_16bit_hex >>=
                    (fun v ->
                        parse (After_dc (bn, 1)) (bdc, v :: adc)))
                <|> ((satisfy is_colon) >>=
                (fun _ -> parse End (bdc, adc)))
            end
            | Before_dc bn when bn = 8 ->
            begin
                read_16bit_hex >>=
                    (fun v ->
                        parse End (v :: bdc, []))
            end
            | After_dc (bn, an) when bn + an < 7 ->
            begin
                ((satisfy is_colon) *> read_16bit_hex) >>=
                    (fun v ->
                        parse (After_dc (bn, (an + 1))) (bdc, v :: adc))
                <|> ((satisfy is_colon) >>=
                (fun _ -> parse End (bdc, adc)))
            end
            | _ -> return (List.rev bdc, List.rev adc)
            in
        parse Start ([],[])
    ;;

    let parser_value_part = parser_value_part_2

    let parser_address =
        lift (fun (part1, part2) -> (List.map int_of_hex_string part1, List.map int_of_hex_string part2))
            (parser_value_part <* end_of_input)
    ;;

    let parser_range =
        lift2 (fun (s_p1, s_p2) (e_p1, e_p2) ->
                (List.map int_of_hex_string s_p1, List.map int_of_hex_string s_p2),
                (List.map int_of_hex_string e_p1, List.map int_of_hex_string e_p2))
        (parser_value_part <* char '-')
        (parser_value_part <* end_of_input)
    ;;

    let parser_network =
        lift2 (fun (s_p1, s_p2) prefix_len ->
                (List.map int_of_hex_string s_p1, List.map int_of_hex_string s_p2), (int_of_string prefix_len))
        (parser_value_part <* char '/')
        (read_7bit_dec <* end_of_input)
    ;;
end
