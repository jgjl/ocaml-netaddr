
open Stdint

type t = uint128

type cur_streak = { cur_streak_start : int; cur_streak_len : int}
type max_streak = { max_streak_start : int; max_streak_len : int}
type streak = { streak_start : int; streak_len : int}

let shift_list = [112;96;80;64;48;32;16;0]

let max_streak_of_cur_streak cur_streak =
  {max_streak_start = cur_streak.cur_streak_start; 
   max_streak_len = cur_streak.cur_streak_len}

let streak_of_cur_streak cur_streak =
  {streak_start = cur_streak.cur_streak_start; 
   streak_len = cur_streak.cur_streak_len}

let streak_of_max_streak max_streak =
  {streak_start = max_streak.max_streak_start; 
   streak_len = max_streak.max_streak_len}

let one = Uint128.one

let mask_16lsb = Uint128.of_string "0xffff"

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

let string_list_to_value hex_elements streak_opt =
  let full_list = match streak_opt with
  | None -> Some (List.map Uint128.of_string hex_elements), None
  | Some streak -> 
    let to_shifted_value e shiftwidth =
      let value = Uint128.of_string e in
        Some Uint128.(shift_left value shiftwidth)
      in
    List.map2 to_shifted_value hex_elements shift_list
    in
  full_list

let of_string s =
  if Pervasives.( (String.length s) > 39 || (String.length s) < 2) then
    None
  else
    begin
      let elements = String.split_on_char ':' s in
      if Pervasives.( List.length elements > 8 || List.length elements < 2 ) then
        None
      else
        try
          let length = List.length elements in
          let split_parts (i, ee, p1, p2) e =
          let hex_element_parts = List.fold_left Pervasives.(fun (i, ee, p1, p2) e -> 
                                                    if String.length e >= 5 then
                                                      raise (Address.Parser_error "String too long.")
                                                    else if (String.length e = 0) then
                                                      begin
                                                        if i = 0 then
                                                          (i+1, ee + 1, None, None)
                                                        else
                                                          if ee > 0 then
                                                            raise (Address.Parser_error "Too many :s.")
                                                          else
                                                            if i = length - 1 then
                                                              (i+1, ee + 1, p1, p2)
                                                            else
                                                              if 
                                                      end
                                                    else
                                                    ("0x" ^ e)) elements in

          print_string (" Parsing, this is what I got: " ^ String.concat " " hex_elements ^ "\n");
          let streak = find_first_longest_streak (String.equal "-") hex_elements in
          let result_value = string_list_to_value hex_elements streak in
          Some result_value
        with
          Address.Parser_error e -> None
    end

let to_string netaddr =
  Uint128.(
    print_string (" int value: " ^ to_string netaddr ^ "\n");
    (*let shift_list = [112;96;80;64;48;32;16;0] in*)
    (* Shift and 'and' each 16bit part of the value*)
    let b16_values = List.map (fun sw -> logand (shift_right netaddr sw) mask_16lsb) shift_list in
    (* Find the longest list of conscutive zeros to be replaced by :: in the output *)
    (* Convert value to hex, remove '0x' prefix *)
    let uint_to_hex v = 
      let hex_raw = Uint128.to_string_hex v in
      let hex_raw_len = String.length hex_raw in
      String.sub hex_raw 2 Pervasives.(hex_raw_len-2)
      in
    let string_fold_fun = match find_first_longest_streak (fun e -> Uint128.(compare e zero) = 0) b16_values with
      (* No list of consecutive zeros found, just print every element separated by ':' *)
      | None ->
        (fun (a,i) v -> match i with
                        | 0 -> to_string_hex v, Pervasives.(succ i)
                        | n -> a ^ (to_string_hex v), Pervasives.(succ i)
        )
      (* List of consecutive zeros found, replace it by '::', print every element separated 
          by ':' elsewhere *)
      | Some streak -> 
        Pervasives.(
          print_string ("\n Streak start: " ^ (string_of_int streak.streak_start) ^ "\n");
          print_string (" Streak length: " ^ (string_of_int streak.streak_len) ^ "\n");
          (fun (a,i) v -> if (i = 0 && streak.streak_start = 0) || 
                            (streak.streak_start < i && (streak.streak_start+streak.streak_len-1) > i) then
                            a, succ i
                          else if (streak.streak_start < i) && (streak.streak_start+streak.streak_len-1) = i then
                            begin
                              print_string ("Hallo i=" ^ Pervasives.string_of_int i ^ "\n");
                              if i = 7 && v = zero then
                                (a ^ "::"), succ i
                              else
                                (a ^ "::" ^ uint_to_hex v), succ i
                            end
                          else
                            (a ^ ":" ^ uint_to_hex v), succ i
          )
        ) in
    match List.fold_left string_fold_fun ("", 0) b16_values with
    | address_string, 8 -> address_string
    | _, i -> raise (Address.Parser_error ("Did not get 8 elements but " ^ string_of_int i))
    )

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