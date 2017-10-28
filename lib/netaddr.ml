open Stdint

module type NetAddress = sig
  type t
  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val of_string : string -> t option
  val to_string : t -> string
end


module IPv4 : NetAddress = struct
  open Uint32

  type t = uint32

  let uint_8b1_0b0 = of_string "0xff"
  let uint_8b1_8b0 = of_string "0xff00"
  let uint_8b1_16b0 = of_string "0xff0000"
  let uint_8b1_24b0 = of_string "0xff000000"

  let (<) (a) b =
    (compare a b) < 0

  let (>) a b =
    (compare a b) > 0

  let of_string s =
    if Pervasives.(>) (String.length s) 15 
    || Pervasives.(<) (String.length s) 7 then
      None
    else
      begin
        let elements = String.split_on_char '.' s in
        if List.length elements = 4 then
          begin
            let join_elements acc element = 
              match acc with
              | None -> None
              | Some acc_value ->
                begin
                  try
                    let byte_value = of_string element in
                    if compare byte_value zero >= 0 
                    && compare byte_value (Uint32.of_int 255) <= 0 then
                      Some (logor (shift_left acc_value 8) byte_value)
                    else
                      None
                  with
                    _ -> None
                end in 
            List.fold_left join_elements (Some zero) elements
          end
        else
          None
      end

  let to_string addr =
    let b0 = logand addr uint_8b1_0b0 in
    let b1 = shift_right (logand addr uint_8b1_8b0) 8 in
    let b2 = shift_right (logand addr uint_8b1_16b0) 16 in
    let b3 = shift_right (logand addr uint_8b1_24b0) 24 in
    to_string b3 ^"."^ to_string b2 ^"."^ to_string b1 ^"."^ to_string b0

end

(*
module IPv6 : NetAddress = struct
  open Uint128

  type t = uint128

  let uint_16b1_0b0 = of_string "0xffff"

  let (<) (a) b =
    (compare a b) < 0

  let (>) a b =
    (compare a b) > 0

  type state = {
    total_length : int;
    list_length : int;
    index : int;
    empty_elem_list : int list;
    result : uint128;
    error : bool;
  }

  let parse_element_list element_list =
    let update_state state elem_list =
      match elem_list with
      | [] -> Pervasives.({state with index = state.index + 1;})
      | "" :: xs -> 
          Pervasives.({state with empty_elem_list = state.index :: state.empty_elem_list})
      | e :: xs ->
        try
          let value = of_string e in
          let shiftwidth = Pervasives.(state.index * 16) in
          let new_result = logor (shift_left value shiftwidth) state.result in
            Pervasives.({state with result = new_result})
        with
          | _ ->
            Pervasives.({state with index = state.index + 1;
                                    error = true})
    in
    let check_state state =
      if Pervasives.(state.list_length > state.total_length) then
        None
      else
        None
    in
    let rec loop state elem_list =
      let new_state = update_state state elem_list in
      match check_state new_state with
      | None -> None
      | Some state -> 
    in
    let list_length = List.length element_list in
    printf "Total lenght = %d" list_length;
    let state = {
      total_length = 8;
      list_length = list_length;
      index = 0;
      empty_elem_list = [];
      result = zero;
      error = false;
    } in
    match check_state state with
    | None -> None
    | Some new_state -> loop new_state element_list

  let of_string s =
    if Pervasives.(>) (String.length s) 39 then
      None
    else
      let element_list = List.rev (String.split ~on:':' s) in
      parse_element_list element_list

  let to_string addr =
    ""
    (*
    let b0 = logand addr uint_16b1_0b0 in
    let b1 = shift_right (logand addr uint_16b1_16b0) 8 in
    let b2 = shift_right (logand addr uint_16b1_32b0) 16 in
    let b3 = shift_right (logand addr uint_16b1_48b0) 24 in
    to_string b3 ^"."^ to_string b2 ^"."^ to_string b1 ^"."^ to_string b0
    *)

  let opt_to_string opt_addr =
    match opt_addr with
    | None -> "None"
    | Some addr -> to_string addr

end

*)