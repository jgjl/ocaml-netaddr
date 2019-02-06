
exception Result_out_of_range of string
exception Parser_error of string

module type Address = sig
  type a

  val bit_size : int
  val zero : a
  val one : a
  (* val max : a *)
  val logand : a -> a -> a
  (* val logor : a -> a -> a *)
  (* val logxor : a -> a -> a *)
  (* val lognot : a -> a *)
  val shift_left : a -> int -> a
  (* val shift_right : a -> int -> a *)
  val compare : a -> a -> int
  val ( < ) : a -> a -> bool
  (* val ( > ) : a -> a -> bool *)
  (* val of_string : string -> a option *)
  val to_string : a -> string
  val serialize : Faraday.t -> a -> unit
  (* val to_string_bin : a -> string *)
  (* val to_string_oct : a -> string *)
  (* val to_string_hex : a -> string *)
  (* val of_bin_list : int list -> a option *)
  (* val to_bin_list : a -> int list *)
  (* val add_int : a -> int -> a *)
  (* val sub_int : a -> int -> a *)
  val add : a -> a -> a
  val sub : a -> a -> a
  (*val get_bit : a -> int -> bool*)
end


module MakeAddress (N:Stdint.Int) = struct
  let bit_size = N.bits
  let zero = N.zero
  let one = N.one
  let max = N.max_int
  let logand = N.logand
  let logor = N.logor
  let logxor = N.logxor
  let lognot = N.lognot
  let shift_left = N.shift_left
  let shift_right = N.shift_right_logical
  let compare = N.compare
  let to_string_bin = N.to_string_bin
  let to_string_oct = N.to_string_oct
  let to_string_hex = N.to_string_hex
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
  ;;

  let sub netaddr subtrahend =
    N.(
      if netaddr < subtrahend then
        raise (Result_out_of_range (Printf.sprintf "%s - %s < 0" (to_string netaddr) (to_string subtrahend)))
      else
        netaddr - subtrahend
    )
  ;;

  let add_int netaddr summand =
    add netaddr (N.of_int summand)
  ;;

  let sub_int netaddr subtrahend =
    sub netaddr (N.of_int subtrahend)
  ;;

  let get_bit netaddr index =
    N.(
       (compare (logand (shift_right netaddr Pervasives.(bits - index)) one) one) == 0
    )
  ;;

  let to_bin_list address =
    let rec extract_lsb i value =
      if Pervasives.(i = 0) then
        []
      else
        let next_value = N.shift_right value 1 in
        N.to_int (N.logand N.one value) :: (extract_lsb Pervasives.(i-1) next_value)
      in
    List.rev (extract_lsb N.bits address)
  ;;
  
  let of_bin_list bin_list = 
    if Pervasives.((List.length bin_list) > N.bits) then None
    else
    let two = N.add N.one N.one in
    try Some (List.fold_left (fun a e ->
      let new_acc = N.mul a two in
      let e_value = (match e with 
      | 0 -> N.zero 
      | 1 -> N.one 
      | _ -> raise (Parser_error ("Expected binary value, got " ^ (string_of_int e))))
        in
      (N.add new_acc e_value)
    ) N.one bin_list)
    with
      Parser_error _ -> None
  ;;
    
  let of_bytes_big_endian = N.of_bytes_big_endian
end

module MakeRange (A:Address) = struct
  type r = {r_first: A.a; r_last: A.a}

  let make first last =
    if A.(first < last) then
      Some {r_first = first; r_last = last}
    else
      None
  ;;

  let to_string range =
    (A.to_string range.r_first) ^ "-" ^ (A.to_string range.r_last)
  ;;

  let serialize t range =
    let open Faraday in
    A.serialize t range.r_first; 
    write_char t '-'; 
    A.serialize t range.r_last
  ;;

  let get_address range = 
    range.r_first
  ;;

  let get_last_address range = 
    range.r_last
  ;;

  let size range =
    A.sub range.r_last range.r_first
  ;;

  let contains range address =
    if (A.compare range.r_first address) <= 0 &&
        (A.compare range.r_last address) >= 0 then
      true
    else
      false
  ;;

  let contains_range range1 range2 =
    contains range1 range2.r_first && contains range1 range2.r_last
  ;;
end

module MakeNetwork (A:Address) = struct
  type n = {n_first: A.a; n_last: A.a; prefix_len: int}

  let get_address n =
    n.n_first
  ;;

  let get_last_address n =
    n.n_last
  ;;

  let to_string network =
    A.to_string network.n_first ^ "/" ^ string_of_int network.prefix_len
  ;;

  let serialize t network =
    let open Faraday in
    A.serialize t network.n_first; 
    write_char t '/'; 
    write_string t (string_of_int network.prefix_len)
  ;;

  let prefix_len network =
    network.prefix_len
  ;;

  let make first prefix_len =
    let network_size_bits = A.bit_size - prefix_len in
    let network_size = A.(sub (shift_left A.one network_size_bits) A.one) in
    let last = A.(add first network_size) in
    let divisible = A.(logand first network_size) in
    if A.(compare divisible zero) == 0 then
      Some {n_first= first; n_last=last; prefix_len= prefix_len}
    else 
      None
  ;;

  let contains network address =
    if (A.compare network.n_first address) <= 0 &&
        (A.compare network.n_last address) >= 0 then
      true
    else
      false
  ;;

  let contains_network network subnet =
    contains network subnet.n_first && contains network subnet.n_last
  ;;
end


module Eui48 = struct
  open Stdint

  let mask_8lsb = Uint48.of_string "0xff"

  type a = uint48

  include MakeAddress(Uint48)

  let uint_to_hex v = 
    let hex_raw = Uint48.to_string_hex v in
    let hex_raw_len = String.length hex_raw in
    String.sub hex_raw 2 Pervasives.(hex_raw_len-2)
  ;;

  let of_strings b1 b2 b3 b4 b5 b6 =
    (int_of_string b6) lor
    (((int_of_string b5) lsl 8) lor
    (((int_of_string b4) lsl 16) lor
    (((int_of_string b3) lsl 24) lor
    (((int_of_string b2) lsl 32) lor
    ((int_of_string b1) lsl 40)))))

  let of_parsed_value (b1, b2, b3, b4, b5, b6) =
    Uint48.(
      logor (of_int b6)
      (logor (shift_left (of_int b5) 8)
      (logor (shift_left (of_int b4) 16)
      (logor (shift_left (of_int b3) 24)
      (logor (shift_left (of_int b2) 32)
            (shift_left (of_int b1) 40)))))
    )

  let of_string s =
    Pervasives.(
      if (String.length s) > Netaddress_parser.Eui48.max_str_length_address
      || (String.length s) < Netaddress_parser.Eui48.min_str_length_address then
        None
      else
        match Angstrom.parse_string Netaddress_parser.Eui48.parser_address s with
        | Result.Ok result -> Some (of_parsed_value result)
        | _ -> None
    )

  let to_string netaddr =
    Uint48.(
      let b1 = logand netaddr mask_8lsb in
      let b2 = logand (shift_right netaddr 8) mask_8lsb in
      let b3 = logand (shift_right netaddr 16) mask_8lsb in
      let b4 = logand (shift_right netaddr 24) mask_8lsb in
      let b5 = logand (shift_right netaddr 32) mask_8lsb in
      let b6 = logand (shift_right netaddr 40) mask_8lsb in
      (uint_to_hex b6 ^":"^ uint_to_hex b5 ^":"^ uint_to_hex b4 ^":"^ uint_to_hex b3 ^":"^ uint_to_hex b2 ^":"^ uint_to_hex b1)
    )
  
  let serialize t netaddr =
    let open Faraday in
    Uint48.(
      let b1 = logand netaddr mask_8lsb in
      let b2 = logand (shift_right netaddr 8) mask_8lsb in
      let b3 = logand (shift_right netaddr 16) mask_8lsb in
      let b4 = logand (shift_right netaddr 24) mask_8lsb in
      let b5 = logand (shift_right netaddr 32) mask_8lsb in
      let b6 = logand (shift_right netaddr 40) mask_8lsb in
      write_string t (uint_to_hex b6);
      write_char t ':';
      write_string t (uint_to_hex b5);
      write_char t ':';
      write_string t (uint_to_hex b4);
      write_char t ':';
      write_string t (uint_to_hex b3);
      write_char t ':';
      write_string t (uint_to_hex b2);
      write_char t ':';
      write_string t (uint_to_hex b1);
    )

  let of_int = Uint48.of_int
  let to_int = Uint48.to_int

  let of_std_uint48 v = v
end


module IPv4 = struct
  open Stdint

  let mask_8lsb = Uint32.of_string "0xff"

  type t = uint32

  module Address = struct
    type a = t
    include MakeAddress(Uint32)

    let of_parsed_value (b1, b2, b3, b4) =
      Uint32.(logor (of_int b4)
              (logor (shift_left (of_int b3) 8)
              (logor (shift_left (of_int b2) 16)
                    (shift_left (of_int b1) 24))))

    let of_strings b1 b2 b3 b4 =
         ((int_of_string b4) lor
        (((int_of_string b3) lsl 8) lor
        (((int_of_string b2) lsl 16) lor
         ((int_of_string b1) lsl 24))))

    let of_string s =
      Pervasives.(
        if (String.length s) > Netaddress_parser.IPv4.max_str_length_address
        || (String.length s) < Netaddress_parser.IPv4.min_str_length_address then
          None
        else
          match Angstrom.parse_string Netaddress_parser.IPv4.parser_address s with
          | Result.Ok result -> Some (of_parsed_value result)
          | _ -> None
      )

    let to_string netaddr =
      Uint32.(
        let b0 = logand netaddr mask_8lsb in
        let b1 = logand (shift_right netaddr 8) mask_8lsb in
        let b2 = logand (shift_right netaddr 16) mask_8lsb in
        let b3 = logand (shift_right netaddr 24) mask_8lsb in
        (to_string b3 ^"."^ to_string b2 ^"."^ to_string b1 ^"."^ to_string b0)
      )

    let serialize t netaddr =
      let open Faraday in
      Uint32.(
        let b0 = logand netaddr mask_8lsb in
        let b1 = logand (shift_right netaddr 8) mask_8lsb in
        let b2 = logand (shift_right netaddr 16) mask_8lsb in
        let b3 = logand (shift_right netaddr 24) mask_8lsb in
        write_string t (to_string b3);
        write_char t '.';
        write_string t (to_string b2);
        write_char t '.'; 
        write_string t (to_string b1);
        write_char t '.';
        write_string t (to_string b0);
      )

    let of_int = Uint32.of_int
    let to_int = Uint32.to_int
    let of_int32 = Uint32.of_int32
    let to_int32 = Uint32.to_int32

    let of_std_uint32 v = v
  end

  module Range = struct
    include MakeRange(Address)

    let of_string s =
      if Pervasives.((String.length s) > Netaddress_parser.IPv4.max_str_length_range
                  || (String.length s) < Netaddress_parser.IPv4.min_str_length_range) then
        None
      else
        match Angstrom.parse_string Netaddress_parser.IPv4.parser_range s with
        | Result.Ok (first_value, last_value) ->
          make (Address.of_parsed_value first_value) (Address.of_parsed_value last_value)
        | _ -> None
  end
  module Network = struct
    include MakeNetwork(Address)

    let of_string network_string =
      let network_string_len = String.length network_string in
      if Pervasives.(network_string_len > Netaddress_parser.IPv4.max_str_length_network
                  || network_string_len < Netaddress_parser.IPv4.min_str_length_network) then
        None
      else
        let network_range = Angstrom.parse_string Netaddress_parser.IPv4.parser_network network_string in
        match network_range with
        | Result.Error _ -> None
        | Result.Ok (parsed_network_address, prefix_len) ->
          let network_address = Address.of_parsed_value parsed_network_address in
          make network_address prefix_len
  end
end

module IPv6 = struct
  open Stdint

  type t = uint128

  (* type format =
    | Ipv6Short
    | Ipv6Long
    | Ipv6Full *)

  let shift_list = [112;96;80;64;48;32;16;0]

  type cur_streak = { cur_streak_start : int; cur_streak_len : int}
  type max_streak = { max_streak_start : int; max_streak_len : int}
  type streak = { streak_start : int; streak_len : int}

  let mask_16lsb = Uint128.of_string "0xffff"
  let mask_32lsb = Uint128.of_string "0xffffffff"
  let mask_48lsb = Uint128.of_string "0xffffffffffff"
  let mask_third_16lsb = Uint128.of_string "0xffff00000000"

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
                | Some last_max when last_cur.cur_streak_len > last_max.max_streak_len -> 
                  None, Some (max_streak_of_cur_streak last_cur)
                | Some _ -> 
                  None, Some {max_streak_start = last_cur.cur_streak_start; 
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
      List.fold_left (fun a e -> 
                        Uint128.(
                          logor (shift_left a 16) (of_int e)
                        )) Uint128.zero list

    let of_parsed_value (part1, part2) =
      Pervasives.(
        let missing_length = 8 - ((List.length part1) + (List.length part2)) in
        if missing_length = 0 then
          Some (ints_to_value (part1 @ part2))
        else
          let complete_int_list = (List.concat [part1; List.init missing_length (fun _ -> 0); part2]) in
          Some (ints_to_value complete_int_list)
      )

    let of_string ipv6_string =
      let ipv6_string_len = String.length ipv6_string in
      if Pervasives.(ipv6_string_len > Netaddress_parser.IPv6.max_str_length_address 
                  || ipv6_string_len < Netaddress_parser.IPv6.min_str_length_address) then
        None
      else
        let parsed_ipv6 = Angstrom.parse_string Netaddress_parser.IPv6.parser_address ipv6_string in
        match parsed_ipv6 with
        | Result.Error _ -> None
        | Result.Ok (part1, part2) -> of_parsed_value (part1, part2)

    let to_string netaddr =
      (*let shift_list = [112;96;80;64;48;32;16;0] in*)
      (* Shift and 'and' each 16bit part of the value*)
      let b16_values = List.map 
                        (fun sw -> Uint128.(logand (shift_right netaddr sw) mask_16lsb)) 
                        shift_list in
      (* Find the longest list of conscutive zeros to be replaced by :: in the output *)
      (* Convert value to hex, remove '0x' prefix *)
      let uint_to_hex v = 
        let hex_raw = Uint128.to_string_hex v in
        let hex_raw_len = String.length hex_raw in
        String.sub hex_raw 2 Pervasives.(hex_raw_len-2)
        in
      let value_list = List.map uint_to_hex b16_values in
      match find_first_longest_streak (fun e -> Uint128.(compare e zero) = 0) b16_values with
        (* No list of consecutive zeros found, just print every element separated by ':' *)
        | None ->
          String.concat ":" value_list
        (* List of consecutive zeros found, replace it by '::', print every element separated 
            by ':' elsewhere *)
        | Some streak -> 
          begin
            let value_array = Array.of_list value_list in
            let part2_start = streak.streak_start + streak.streak_len in
            let part2_len = (Array.length value_array) - part2_start in
            let part1 = Array.to_list (Array.sub value_array 0 streak.streak_start) in
            let part2 = Array.to_list (Array.sub value_array part2_start part2_len) in
            String.concat ":" part1 ^ "::" ^ String.concat ":" part2
          end

    type either =
    | Value of int
    | Streak of int

    let serialize t netaddr =
      (* [@tailcall] *)
      let rec detect_streak (lso: int option) (cso: int option) (r: either list) (i: int list) : int option * either list =
        match i with
        | [] ->
        begin
          match lso, cso with
          | None   , Some _                            ->  cso, r
          | Some ls, Some cs when Pervasives.(cs > ls) ->  cso, r
          | None   , None                              -> None, r
          | Some _ , None                              ->  lso, r
          | Some _ , Some _                            ->  lso, r
        end
        | ni :: ri -> 
        begin
          let vi = Uint128.(to_int (logand (shift_right netaddr ni) mask_16lsb)) in
          if vi = 0 then
            match lso, cso with
            | None  , None    -> detect_streak None (Some 1) r ri
            | Some _, None    -> detect_streak lso  (Some 1) r ri
            | None  , Some cs -> detect_streak None (Some (cs + 1)) r ri
            | Some _, Some cs -> detect_streak lso  (Some (cs + 1)) r ri
          else
            match lso, cso with
            | None   , None                              -> detect_streak None None ((Value vi) :: r) ri
            | Some _ , None                              -> detect_streak  lso None ((Value vi) :: r) ri
            | None   , Some cs                           -> detect_streak  cso None ((Value vi) :: (Streak cs) :: r) ri
            | Some ls, Some cs when Pervasives.(cs > ls) -> detect_streak  cso None ((Value vi) :: (Streak cs) :: r) ri
            | Some ls, Some _                            -> detect_streak  lso None ((Value vi) :: (Streak ls) :: r) ri
        end
        in
      let rec combine (rlso: int option) (rl: either list) first =
        let open Faraday in
        match rl with
        | [] -> ()
        | head :: tails ->
            match rlso, head, first with
            | None,    Streak _ , _               -> raise (Parser_error "ERROR: found streak where none should be.")
            | None,     Value v, true            -> 
            begin
                write_string t (string_of_int v);
                combine None tails false
            end
            | None,     Value v, false           -> 
            begin
                write_char t ':';
                write_string t (string_of_int v);
                combine None tails false
            end
            | Some ls, Streak sl, _ when ls = sl -> 
            begin
                write_char t ':';
                combine None tails false
            end
            | Some _, Streak sl, _ -> 
            begin
                let rec print_streak tsl =
                    if tsl = 0 then
                        write_string t "0"
                    else
                    (
                        write_string t "0:";
                        print_streak (tsl - 1)
                    )
                    in
                print_streak sl;
                combine rlso tails first
            end
            | Some _,   Value v, true            ->
            begin
                write_string t (string_of_int v);
                combine rlso tails false
            end
            | Some _,   Value v, false           ->
            begin
                write_char t ':';
                write_string t (string_of_int v);
                combine rlso tails false
            end
            in
      let min_streak, value_list = detect_streak None None [] shift_list in
      combine min_streak value_list true

    let of_ipv4_address ipv4_address =
      let ipv4_value = Uint128.of_uint32 ipv4_address in
      Uint128.logor mask_third_16lsb ipv4_value

    let to_ipv4_address address =
      if Uint128.(compare (logand mask_48lsb address) address) = 0 &&
        Uint128.(compare (logand mask_third_16lsb address) mask_third_16lsb) = 0 then
        Some (Uint128.to_uint32 (Uint128.logand mask_32lsb address))
      else
        None
      
    let of_std_uint128 v = v
  end

  module Range = struct
    include MakeRange(Address)

    let of_string range_string =
      let range_string_len = String.length range_string in
      if Pervasives.(range_string_len > Netaddress_parser.IPv6.max_str_length_range 
                  || range_string_len < Netaddress_parser.IPv6.min_str_length_range) then
        None
      else
        let parsed_range = Angstrom.parse_string Netaddress_parser.IPv6.parser_range range_string in
        match parsed_range with
        | Result.Error _ -> None
        | Result.Ok (first_parsed,last_parser) ->
          let first = Address.of_parsed_value first_parsed in
          let last = Address.of_parsed_value last_parser in
          match first, last with
          | Some first_address, Some last_address ->
            make first_address last_address
          | _, _ -> None
  end

  module Network = struct
    include MakeNetwork(Address)

    let of_string network_string =
      let network_string_len = String.length network_string in
      if Pervasives.(network_string_len > Netaddress_parser.IPv6.max_str_length_network 
                  || network_string_len < Netaddress_parser.IPv6.min_str_length_network) then
                  (
        Printf.printf "ERROR: string of wrong size"; None
                  )
      else
        let network_range = Angstrom.parse_string Netaddress_parser.IPv6.parser_network network_string in
        match network_range with
        | Result.Error e -> Printf.printf "ERROR: %s" e; None
        | Result.Ok (parsed_network_address, prefix_len) ->
          let network_address_opt = Address.of_parsed_value parsed_network_address in
          match network_address_opt with
          | None -> None
          | Some network_address -> 
            make network_address prefix_len
  end
end

