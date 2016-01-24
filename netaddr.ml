open Core.Std
open Stdint

module type NetAddress = sig
  type t
  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val of_string : string -> t option
  val to_string : t -> string
  val opt_to_string : t option -> string
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

  let addr_from_bytes (b3,b2,b1,b0) =
    if (b0 <= uint_8b1_0b0) && (b1 <= uint_8b1_0b0) && (b2 <= uint_8b1_0b0) && (b3 <= uint_8b1_0b0) then
      Some (
        shift_left b3 24
        |> logor (shift_left b2 16)
        |> logor (shift_left b1 8)
        |> logor b0
        )
    else
      None

  let bytes_from_string s =
    match String.split ~on:'.' s with
      | [] -> None
      | _ :: [] -> None
      | _ :: _ :: [] -> None
      | _ :: _ :: _ :: [] -> None
      | b3 :: b2 :: b1 :: b0 :: []->
        (
          try
            Some (of_string b3,
                  of_string b2,
                  of_string b1,
                  of_string b0)
          with
            | _ -> None
        )
      | _ :: _ :: _ :: _ :: _ -> None

  let of_string s =
    if Pervasives.(>) (String.length s) 15 then
      None
    else
      match bytes_from_string s with
      | None -> None
      | Some bytes_tuple -> addr_from_bytes bytes_tuple

  let to_string addr =
    let b0 = logand addr uint_8b1_0b0 in
    let b1 = shift_right (logand addr uint_8b1_8b0) 8 in
    let b2 = shift_right (logand addr uint_8b1_16b0) 16 in
    let b3 = shift_right (logand addr uint_8b1_24b0) 24 in
    to_string b3 ^"."^ to_string b2 ^"."^ to_string b1 ^"."^ to_string b0

  let opt_to_string opt_addr =
    match opt_addr with
    | None -> "None"
    | Some addr -> to_string addr

end

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
