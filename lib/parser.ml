
open Stdint
open Angstrom

type parsed_ipv4 = int * int * int * int

type parsed_ipv6 = int list * int list

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
        | Some c-> fail ("end_of_string, next char: " ^ (String.make 1 c))

type read_byte_state =
| RBS_start
| RBS_01
| RBS_2
| RBS_39
| RBS_2_5
| RBS_r_09
| RBS_end

let read_byte =
  (*
    Design as state machine.
    Accept is implicitly enabled for all states by the default -> None transition.
  *)
  (scan RBS_start 
    (fun (state) c -> 
        match state, c with
        | RBS_start, '0' .. '1' -> Some RBS_01
        | RBS_start, '2' -> Some RBS_2
        | RBS_start, '3' .. '9' -> Some RBS_39
        | RBS_01, '0' .. '9' -> Some RBS_r_09
        | RBS_r_09, '0' .. '9' -> Some RBS_end
        | RBS_39, '0' .. '9' -> Some RBS_end
        | RBS_2, '0' .. '4' -> Some RBS_r_09
        | RBS_2, '5' -> Some RBS_2_5
        | RBS_2, '6' .. '9' -> Some RBS_end
        | RBS_2_5, '0' .. '5' -> Some RBS_end
        | _, _ -> None
        )) >>= function 
                | result, RBS_start -> fail "Could not read byte value"
                | result, _ -> return result 

let read_16bit =
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
  (*
  else if c = m then 
    return ([],[])
  *)
  (* TODO: write FSM, then encode here *)
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
    (*
    <|> return ([],[])
    *)


let stringbytes_to_list b1 b2 b3 b4 =
    [int_of_string b1; int_of_string b2; int_of_string b3; int_of_string b4]

let parser_ipv4_part = 
  (lift4 
    (fun b1 b2 b3 b4 -> int_of_string b1, int_of_string b2, int_of_string b3, int_of_string b4)
    (read_byte <* (skip is_dot))  
    (read_byte <* (skip is_dot)) 
    (read_byte <* (skip is_dot)) 
     read_byte)

let parser_ipv4 = 
  parser_ipv4_part <* end_of_input

let parse_ipv4 ipv4_string =
    match parse_string parser_ipv4 ipv4_string with
    | Result.Ok (b1,b2,b3,b4) -> (string_of_int b1) ^ "." ^ string_of_int b2 ^ "." ^ string_of_int b3 ^ "." ^ string_of_int b4
    | Result.Error message -> message

let int_of_hex_string s =
    int_of_string ("0x" ^ s)


(*
let merge_ffff_dq m (b1, b2, b3, b4) =
    let second_last = (b1 lsl 8) lor b2 in
    let last = (b3 lsl 8) lor b4 in
    let part2 = [int_of_hex_string m; second_last; last] in
    ParsedIpv6TwoParts ([], part2)

()
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
*)

let parser_ipv6_part_new =
  at_most_split 0 8 
    (read_16bit <* (satisfy is_colon)) 
    ((satisfy is_colon) *> read_16bit) 
    read_16bit
    (*
    (read_16bit <|> (peek_string 1 >>= function | ":" -> return "" | s -> fail s ))
    *)
    (string ":")

let parser_ipv6 = 
  (*
  parser_ipv6_part <* end_of_input
  *)
  (parser_ipv6_part_new <* end_of_input)
  >>| 
  (fun (part1, part2) -> (List.map int_of_hex_string part1, List.map int_of_hex_string part2))

let parser_ipv6_new = 
  parser_ipv6_part_new <* end_of_input


let parse_ipv6 ipv6_string =
  (*
  match parse_string parser_ipv6 ipv6_string with
  | Result.Ok (ParsedIPv6Complete result) -> String.concat "-:-" (List.map string_of_int result)
  | Result.Ok ParsedIpv6TwoParts (result1, result2) -> 
      String.concat "-:-" (List.map string_of_int result1) ^ "::" ^
      String.concat "-:-" (List.map string_of_int result2)
  | Result.Error message -> "ERROR: " ^ message
  *)
  match parse_string parser_ipv6_new ipv6_string with
  | Result.Ok (result1, result2) -> 
      String.concat "-:-" result1 ^ "::" ^
      String.concat "-:-" result2
  | Result.Error message -> "ERROR: " ^ message


(*
let parse_ipv6_new ipv6_string =
  match parse_string parser_ipv6 ipv6_string with
  | Result.Ok (result, r2) -> String.concat "-:-" result ^ "-Middle-" ^ String.concat "-:-" r2
  | Result.Error message -> "ERROR: " ^ message
  *)
