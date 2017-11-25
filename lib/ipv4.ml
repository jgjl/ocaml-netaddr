
open Stdint

type t = uint32

let one = Uint32.one

let mask_8lsb = Uint32.of_string "0xff"

let (<) (a) b =
  (Uint32.compare a b) < 0

let (>) a b =
  (Uint32.compare a b) > 0

let strings_to_value byte_list =
  (*
  let shift_list = [24;16;8;0] in
  List.fold_left2 (fun acc shiftwidth bytevalue -> 
                    Uint32.(logor acc (shift_left (of_int bytevalue) shiftwidth))
                    )
                  Uint32.zero shift_list byte_list
  *)
  match byte_list with
  | b1 :: b2 :: b3 :: b4 :: empty -> 
    Uint32.(logor (of_int b4) 
           (logor (shift_left (of_int b3) 8) 
           (logor (shift_left (of_int b2) 16) 
                  (shift_left (of_int b1) 24))))
  | _ -> raise (Address.Parser_error "Parser failed")

let of_string s : t option =
  if (String.length s) >= 16 
  || (String.length s) <= 6 then
    None
  else
    match Angstrom.parse_string Parser.parser_ipv4 s with
    | Result.Ok result -> Some (strings_to_value result)
    | _ -> None

let to_string netaddr =
  Uint32.(
    let b0 = logand netaddr mask_8lsb in
    let b1 = logand (shift_right netaddr 8) mask_8lsb in
    let b2 = logand (shift_right netaddr 16) mask_8lsb in
    let b3 = logand (shift_right netaddr 24) mask_8lsb in
    (to_string b3 ^"."^ to_string b2 ^"."^ to_string b1 ^"."^ to_string b0)
  )

let add netaddr summand =
  Uint32.(
    if (max_int - netaddr) < summand then
      raise (Address.Result_out_of_range (Printf.sprintf "%s + %s > %s" (to_string netaddr) (to_string summand) (to_string max_int)))
    else
      netaddr + summand
  )

let sub netaddr subtrahend =
  Uint32.(
    if netaddr < subtrahend then
      raise (Address.Result_out_of_range (Printf.sprintf "%s - %s < 0" (to_string netaddr) (to_string subtrahend)))
    else
      netaddr - subtrahend
  )

let add_int netaddr summand =
  add netaddr (Uint32.of_int summand)

let sub_int netaddr subtrahend =
  sub netaddr (Uint32.of_int subtrahend)
