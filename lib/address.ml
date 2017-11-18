
exception Result_out_of_range of string
exception Parser_error of string


(*
module type Address = sig
  type t
  val one : t
  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val of_string : string -> t option
  val to_string : t -> string
  val add_int : t -> int -> t
  val sub_int : t -> int -> t
  val add : t -> t -> t
  val sub : t -> t -> t
end
*)

(*
module type IPv4 = sig
  type t
  include Address with type t := t
end

module type IPv6 = sig
  type t
  include Address with type t := t
  (*
  val of_ipv4 : IPv4.t -> t
  *)
end

(*
type ipaddress =
  | IPv4Address of IPv4.t
  | IPv6Address of IPv6.t
  *)

module IPv4 : IPv4 = struct
  type t = uint32

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
            let b0 = of_string elements.(0) in
            let b1 = of_string elements.(1) in
            let b2 = of_string elements.(2) in
            let b3 = of_string elements.(3) in
            if b0 <= max_byte && b1 <= max_byte && b2 <= max_byte && b3 <= max_byte then
              Some (logor b0 (logor (shift_left b1 8) (logor (shift_left b2 16) (shift_left b3 24))))
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
        raise (Result_out_of_range (Printf.sprintf "%s + %s > %s" (to_string netaddr) (to_string summand) (to_string max_int)))
      else
        netaddr + summand
    )

  let sub netaddr subtrahend =
    Uint32.(
      if netaddr < subtrahend then
        raise (Result_out_of_range (Printf.sprintf "%s - %s < 0" (to_string netaddr) (to_string subtrahend)))
      else
        netaddr - subtrahend
    )

  let add_int netaddr summand =
    add netaddr (Uint32.of_int summand)

  let sub_int netaddr subtrahend =
    sub netaddr (Uint32.of_int subtrahend)

end


module IPv6 : IPv6 = struct
  type t = uint128

  let one = Uint128.one

  let mask_16lsb = Uint128.of_string "0xffff"

  let (<) (a) b =
    (Uint128.compare a b) < 0

  let (>) a b =
    (Uint128.compare a b) > 0

  let of_string s =
    if Pervasives.( (String.length s) > 39 || (String.length s) < 2) then
      None
    else
      begin
        let elements = String.split_on_char ':' s in
        if Pervasives.( List.length elements > 7 || List.length elements < 2 ) then
          None
        else
          try
            let hex_elements = List.map (fun e -> if String.length e >= 5 then
                                 raise (Parser_error "String too long")
                               else
                                 "0x" ^ e) elements in
            (*
            let _, _, zero_start List.fold_left (fun (i, cur_zero_opt, prev_zeros_start_opt, prev_zero_len_opt) e -> 
                                           match cur_zero_opt, prev_zeros_opt, e with
                                           | None, None, "" -> (i+1, 1, None)
                                           | None, None, _  -> (i+1, None, None)
                                           | Some cur_zero, None, "" when cur_zero >= 2 -> raise (Parser_error "Too many '::'")
                                           | Some cur_zero, None, "" -> (i+1, Some (cur_zero+1), None)
                                           | Some cur_zero, None, _ when cur_zero > 1 -> (i+1, None, Some (i-cur_zero-1))
                                           | Some cur_zero, None, _ -> (i+1, None, None)
                                           | None, Some prev_zero, "" -> (i+1, Some 1, Some prev_zero)
                                           | None, Some prev_zero, _ -> (i+1, None, Some prev_zero)
                                           | Some cur_zero, Some prev_zero, "" -> raise (Parser_error "Too many '::'")
                                           | Some cur_zero, Some prev_zero, _ -> (i+1, None, Some prev_zero)
                                           ) (0, None, None) elements in
            *)
            Some Uint128.zero
          with
            Parser_error e -> None
      end

  let to_string netaddr =
    Uint128.(
      let shift_list = [112;96;80;64;48;32;16;0] in
      (* Shift and 'and' each 16bit part of the value*)
      let b16_values = List.map (fun sw -> logand (shift_right netaddr sw) mask_16lsb) shift_list in
      (* Find the longest list of conscutive zeros to be replaced by :: in the output *)
      let detect_0_list (i, cur_len_opt, cur_start_opt, max_len_opt, max_start_opt) value = 
        Pervasives.(
          if value <> zero then
            (i + 1, None, None, max_len_opt, max_start_opt)
          else
            match cur_len_opt, cur_start_opt, max_len_opt, max_start_opt with
            | None, None, None, None ->
              (i+1, Some 1, Some i, Some 1, Some i)
            | None, None, Some max_len, Some max_start -> 
              (i+1, Some 1, Some i, Some max_len, Some max_start)
            | Some cur_len, Some cur_start, Some max_len, Some max_start when cur_len > max_len ->
              (i+1, Some (cur_len + 1), Some cur_start, Some (cur_len + 1), Some cur_start)
            | Some cur_len, Some cur_start, Some max_len, Some max_start ->
              (i+1, Some (cur_len + 1), Some cur_start, Some max_len, Some max_start)
            | _, _, _, _ -> raise (Parser_error "E0")
         ) in
      (* Convert value to hex, remove '0x' prefix *)
      let uint_to_hex v = 
        let hex_raw = Uint128.to_string_hex v in
        let hex_raw_len = String.length hex_raw in
        String.sub hex_raw 2 Pervasives.(hex_raw_len-2)
        in
      match (List.fold_left detect_0_list (0, None, None, None, None) b16_values) with
        (* No list of consecutive zeros found, just print every element separated by ':' *)
        | _, _, _, None, None ->
          print_string "3";
          let result_string, _ = List.fold_left (fun (a,i) v -> match i with
                            | 0 -> to_string_hex v, Pervasives.(i+1)
                            | n -> a ^ (to_string_hex v), Pervasives.(i+1))
                          ("",0) b16_values in
          result_string
        (* List of consecutive zeros found, replace it by '::', print every element separated 
           by ':' elsewhere *)
        | _, _, _, Some max_len, Some max_start -> 
          print_string "4";
          Pervasives.(
            let result_string, _ = List.fold_left (fun (a,i) v -> 
                              if (i = 0 && max_start = 0) || 
                                (max_start < i && max_start+max_len > i) then
                                a, i+1
                              else if (max_start < i) && (max_start+max_len) = i then
                                (a ^ "::" ^ uint_to_hex v), i+1
                              else
                                (a ^ ":" ^ uint_to_hex v), i+1)
                            ("",0) b16_values in
            result_string
          )
        | a, b, c, d, e -> 
          print_string "5";
          raise (Parser_error "E1")
    )

  let add netaddr summand =
    Uint128.(
      if (max_int - netaddr) < summand then
        raise (Result_out_of_range (Printf.sprintf "%s + %s > %s" (to_string netaddr) (to_string summand) (to_string max_int)))
      else
        netaddr + summand
    )

  let sub netaddr subtrahend =
    Uint128.(
      if netaddr < subtrahend then
        raise (Result_out_of_range (Printf.sprintf "%s - %s < 0" (to_string netaddr) (to_string subtrahend)))
      else
        netaddr - subtrahend
    )

  let add_int netaddr summand =
    add netaddr (Uint128.of_int summand)

  let sub_int netaddr subtrahend =
    sub netaddr (Uint128.of_int subtrahend)

end
*)