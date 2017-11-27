(*
 * TODO: Implement https://tools.ietf.org/html/rfc5952
 *)

open Stdint

type t = uint128

type format =
    | Ipv6Short
    | Ipv6Long
    | Ipv6Full

let shift_list = [112;96;80;64;48;32;16;0]

type cur_streak = { cur_streak_start : int; cur_streak_len : int}
type max_streak = { max_streak_start : int; max_streak_len : int}
type streak = { streak_start : int; streak_len : int}


let one = Uint128.one

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

let (<) (a) b =
  (Uint128.compare a b) < 0

let (>) a b =
  (Uint128.compare a b) > 0

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

let of_string s =
  if Pervasives.( (String.length s) > 39 || (String.length s) < 2) then
    None
  else
    match Angstrom.parse_string Netaddress__Parser.parser_ipv6 s with
    | Result.Ok (Netaddress__Parser.ParsedIPv6Complete result) -> Some (ints_to_value result)
    | Result.Error _ -> None
    | Result.Ok (Netaddress__Parser.ParsedIpv6TwoParts (part1, part2)) ->
      Pervasives.(
        print_string (" len(part1) = " ^ (string_of_int (List.length part1)) ^ " len(part2) = " ^ (string_of_int (List.length part2)) ^ ".\n");
        let missing_length = 8 - ((List.length part1) + (List.length part2)) in
        print_string ("Missing length: " ^ (string_of_int missing_length) ^ ".\n");
        if missing_length < 1 then
          None
        else 
          let complete_int_list = (List.concat [part1; List.init missing_length (fun x -> 0); part2]) in
          print_string ("Complete int list: " ^ (String.concat ":" (List.map string_of_int complete_int_list)) ^ ".\n");
          Some (ints_to_value complete_int_list)
      )

let to_string netaddr ?(format=Ipv6Short) =
    print_string (" int value: " ^ Uint128.to_string netaddr ^ "\n");
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
        print_string ("\n Streak start: " ^ (string_of_int streak.streak_start) ^ "\n");
        print_string (" Streak length: " ^ (string_of_int streak.streak_len) ^ "\n");
        let value_array = Array.of_list (List.map uint_to_hex b16_values) in
        let part2_start = streak.streak_start+streak.streak_len in
        let part2_len = (Array.length value_array) - part2_start in
        let part1 = Array.to_list (Array.sub value_array 0 streak.streak_start) in
        let part2 = Array.to_list (Array.sub value_array part2_start part2_len) in
        String.concat ":" part1 ^ "::" ^ String.concat ":" part2

let add netaddr summand =
  Uint128.(
    if (max_int - netaddr) < summand then
      raise (Address.Result_out_of_range (Printf.sprintf "%s + %s > %s" (to_string netaddr) (to_string summand) (to_string max_int)))
    else
      netaddr + summand
  )

let sub netaddr subtrahend =
  Uint128.(
    if netaddr < subtrahend then
      raise (Address.Result_out_of_range (Printf.sprintf "%s - %s < 0" (to_string netaddr) (to_string subtrahend)))
    else
      netaddr - subtrahend
  )

let add_int netaddr summand =
  add netaddr (Uint128.of_int summand)

let sub_int netaddr subtrahend =
  sub netaddr (Uint128.of_int subtrahend)