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

  let uint_255 = of_int 255
  let uint_256 = of_int 256
  let uint_65280 = of_int 65280
  let uint_16711680 = of_int 16711680
  let uint_4278190080 = of_int (-16777216)

  let (<) (a) b =
    (compare a b) < 0

  let (>) a b =
    (compare a b) > 0

  let addr_from_bytes (b3,b2,b1,b0) =
    if (b0 < uint_256) && (b1 < uint_256) && (b2 < uint_256) && (b3 < uint_256) then
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
    match bytes_from_string s with
    | None -> None
    | Some bytes_tuple -> addr_from_bytes bytes_tuple

  let to_string addr =
    let b0 = logand addr uint_255 in
    let b1 = shift_right (logand addr uint_65280) 8 in
    let b2 = shift_right (logand addr uint_16711680) 16 in
    let b3 = shift_right (logand addr uint_4278190080) 24 in
    to_string b3 ^"."^ to_string b2 ^"."^ to_string b1 ^"."^ to_string b0

  let opt_to_string opt_addr =
    match opt_addr with
    | None -> "None"
    | Some addr -> to_string addr

end
