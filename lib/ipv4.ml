
open Stdint

type t = uint32

module Parser = struct
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

  let parse_address address_string =
    parse_string integer address_string
end

let one = Uint32.one

let mask_8lsb = Uint32.of_string "0xff"

let (<) (a) b =
  (Uint32.compare a b) < 0

let (>) a b =
  (Uint32.compare a b) > 0

let of_string s =
  if (String.length s) >= 16 
  || (String.length s) <= 6 then
    None
  else
    let elements = Array.of_list (String.split_on_char '.' s) in
    if Array.length elements = 4 then
      Uint32.(
        try
          let max_byte = of_int 255 in
          let b3 = of_string elements.(3) in
          let b2 = of_string elements.(2) in
          let b1 = of_string elements.(1) in
          let b0 = of_string elements.(0) in
          if b0 <= max_byte && b1 <= max_byte && b2 <= max_byte && b3 <= max_byte then
            Some (logor b3 (logor (shift_left b2 8) (logor (shift_left b1 16) (shift_left b0 24))))
          else
            None
        with 
        | uint32_of_string -> None
      )
    else
      None

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
