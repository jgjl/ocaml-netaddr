
exception Result_out_of_range of string
exception Parser_error of string

module type Address = sig
  type a

  val one : a
  val compare : a -> a -> int
  val ( < ) : a -> a -> bool
  val ( > ) : a -> a -> bool
  val of_string : string -> a option
  val to_string : a -> string
  val add_int : a -> int -> a
  val sub_int : a -> int -> a
  val add : a -> a -> a
  val sub : a -> a -> a
end

module type Range = sig
  type r

  val of_string : string -> r option
  val to_string : r -> string
  val element_of : r -> bool
  val intersect : r -> r -> r
end

module type Network = sig
  type n

  val of_string : string -> n option
  val to_string : n -> string
  val element_of : n -> bool
  val subnet_of : n -> n -> bool
  val prefix_len : n -> int
end


module MakeAddress (N:Stdint.Int) = struct
  let one = N.one

  let compare a b =
    N.compare a b

  let (<) (a) b =
    (N.compare a b) < 0

  let (>) a b =
    (N.compare a b) > 0

  let add netaddr summand =
    N.(
      if (max_int - netaddr) < summand then
        raise (Result_out_of_range (Printf.sprintf "%s + %s > %s" (to_string netaddr) (to_string summand) (to_string max_int)))
      else
        netaddr + summand
    )

  let sub netaddr subtrahend =
    N.(
      if netaddr < subtrahend then
        raise (Result_out_of_range (Printf.sprintf "%s - %s < 0" (to_string netaddr) (to_string subtrahend)))
      else
        netaddr - subtrahend
    )

  let add_int netaddr summand =
    add netaddr (N.of_int summand)

  let sub_int netaddr subtrahend =
    sub netaddr (N.of_int subtrahend)
end

module MakeRange (A:Address) = struct
  type r = {first: A.a; last: A.a}

  let make first last =
    Some {first= first; last= last}

  let of_string range_string =
    make A.one A.one

  let to_string range =
    (A.to_string range.first) ^ "-" ^ (A.to_string range.last)

  let element_of range address =
    if (A.compare range.first address) <= 0 &&
        (A.compare range.last address) >= 0 then
      true
    else
      false

  let intersect range1 range2 =
    if ((A.compare range1.first range2.first) < 0 &&
        (A.compare range1.first range2.last) < 0) ||
       ((A.compare range1.last range2.first) > 0 &&
        (A.compare range1.last range2.last) > 0) then
      true
    else
      false

  let size range =
    A.sub range.last range.first
end

module MakeNetwork (A:Address) = struct
  type n = {address: A.a; prefix_len: int}

  let to_string network =
    A.to_string network.address ^ "/" ^ string_of_int network.prefix_len

  let prefix_len network =
    network.prefix_len

  let make address prefix_len =
    Some {address= address; prefix_len= prefix_len}

  let subnet_of network subnet =
    false

  let element_of range address =
    false

  let of_string network_string =
    make A.one 0
end


module IPv4 = struct
  open Stdint

  let mask_8lsb = Uint32.of_string "0xff"

  type t = uint32

  module Address = struct
    type a = t
    include MakeAddress(Uint32)

    let strings_to_value (b1, b2, b3, b4) =
      Uint32.(logor (of_int b4)
              (logor (shift_left (of_int b3) 8)
              (logor (shift_left (of_int b2) 16)
                    (shift_left (of_int b1) 24))))

    let of_string s =
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
  end

  module Range = struct
    include MakeRange(Address)
  end
  module Network = struct
    include MakeNetwork(Address)
  end
end

module IPv6 = struct
  open Stdint

  type t = uint128

  type address_object =
    | Ipv6Address
    | Ipv6Network
    | Ipv6Range

  type format =
    | Ipv6Short
    | Ipv6Long
    | Ipv6Full

  let shift_list = [112;96;80;64;48;32;16;0]

  type cur_streak = { cur_streak_start : int; cur_streak_len : int}
  type max_streak = { max_streak_start : int; max_streak_len : int}
  type streak = { streak_start : int; streak_len : int}

  let mask_16lsb = Uint128.of_string "0xffff"

  let max_streak_of_cur_streak cur_streak =
    {max_streak_start = cur_streak.cur_streak_start;
    max_streak_len = cur_streak.cur_streak_len}

  let streak_of_cur_streak cur_streak =
    {streak_start = cur_streak.cur_streak_start;
    streak_len = cur_streak.cur_streak_len}

  let streak_of_max_streak max_streak =
    {streak_start = max_streak.max_streak_start;
    streak_len = max_streak.max_streak_len}


  module Address = struct

    type a = t

    include MakeAddress(Uint128)

    let find_first_longest_streak element_selector element_list =
      Pervasives.(
        let detect_list (i, cur_opt, max_opt) value =
        begin
          let element_selected = element_selector value in
          print_string (" Selected element " ^ string_of_int i ^ " : " ^ string_of_bool element_selected ^ "\n");
          let new_cur, new_max =
            match cur_opt with
            | None ->
            begin
              if element_selected then
                Some {cur_streak_start = i; cur_streak_len = 1}, max_opt
              else
                None, max_opt
            end
            | Some last_cur ->
            begin
              if element_selected then
                Some {last_cur with cur_streak_len = last_cur.cur_streak_len + 1}, max_opt
              else
                match max_opt with
                | None -> None, Some (max_streak_of_cur_streak last_cur)
                | Some last_max when last_cur.cur_streak_len > last_max.max_streak_len-> None, Some (max_streak_of_cur_streak last_cur)
                | Some last_max -> None, Some {max_streak_start = last_cur.cur_streak_start; 
                                                max_streak_len = last_cur.cur_streak_len}
            end in
          (succ i), new_cur, new_max
        end in
        match List.fold_left detect_list (0, None, None) element_list with
        | _, Some cur_streak, None ->
          Some (streak_of_cur_streak cur_streak)
        | _, Some cur_streak, Some max_streak when cur_streak.cur_streak_len > max_streak.max_streak_len ->
          Some (streak_of_cur_streak cur_streak)
        | _, _, Some max_streak ->
          Some (streak_of_max_streak max_streak)
        | _, _, None ->
          None
      )

    let ints_to_value list =
      (*assert (List.length list = 8);*)
      let stringsis = (List.map string_of_int list) in
      print_string (" Int: " ^ (String.concat ":" stringsis) ^ "\n");
      List.fold_left (fun a e ->
                        Uint128.(
                          (*shift_left (logor a (of_int e)) 16*)
                          logor (shift_left a 16) (of_int e)
                        )) Uint128.zero list

    let of_string ipv6_string =
      let ipv6_string_len = String.length ipv6_string in
      if Pervasives.( ipv6_string_len > 39 || ipv6_string_len < 2) then
        None
      else
        let parsed_ipv6 = Angstrom.parse_string Parser.parser_ipv6 ipv6_string in
        match parsed_ipv6 with
        | Result.Error _ -> None
        | Result.Ok (part1, part2) ->
          Pervasives.(
            let missing_length = 8 - ((List.length part1) + (List.length part2)) in
            if missing_length = 0 then
              Some (ints_to_value (part1 @ part2))
            else
              let complete_int_list = (List.concat [part1; List.init missing_length (fun x -> 0); part2]) in
              Some (ints_to_value complete_int_list)
          )

    let to_string netaddr =
      (*let shift_list = [112;96;80;64;48;32;16;0] in*)
      (* Shift and 'and' each 16bit part of the value*)
      let b16_values = List.map (fun sw -> Uint128.(logand (shift_right netaddr sw) mask_16lsb)) shift_list in
      (* Find the longest list of conscutive zeros to be replaced by :: in the output *)
      (* Convert value to hex, remove '0x' prefix *)
      let uint_to_hex v =
        let hex_raw = Uint128.to_string_hex v in
        let hex_raw_len = String.length hex_raw in
        String.sub hex_raw 2 Pervasives.(hex_raw_len-2)
        in
      match find_first_longest_streak (fun e -> Uint128.(compare e zero) = 0) b16_values with
        (* No list of consecutive zeros found, just print every element separated by ':' *)
        | None ->
          String.concat ":" (List.map uint_to_hex b16_values)
        (* List of consecutive zeros found, replace it by '::', print every element separated
            by ':' elsewhere *)
        | Some streak ->
          begin
            let value_array = Array.of_list (List.map uint_to_hex b16_values) in
            let part2_start = streak.streak_start+streak.streak_len in
            let part2_len = (Array.length value_array) - part2_start in
            let part1 = Array.to_list (Array.sub value_array 0 streak.streak_start) in
            let part2 = Array.to_list (Array.sub value_array part2_start part2_len) in
            String.concat ":" part1 ^ "::" ^ String.concat ":" part2
          end

    (*
    let format netaddr ?(out_format=Ipv6Short) =
      to_string netaddr
      *)

    let of_ipv4_address ipv4_address =
      one

    let to_ipv4_address address =
      Some IPv4.Address.one
  end

  module Range = struct
    include MakeRange(Address)
  end
  module Network = struct
    include MakeNetwork(Address)
  end
end

type ipaddress =
  | IPv4Address of IPv4.Address.a
  | IPv6Address of IPv6.Address.a
  | IPv4Range of IPv4.Range.r
  | IPv6Range of IPv6.Range.r
  | IPv4Network of IPv4.Network.n
  | IPv6Network of IPv6.Network.n
