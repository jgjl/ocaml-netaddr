
(*
    Text representation of IPv6 addresses
    https://tools.ietf.org/html/rfc4291
*)

type format =
  | Ipv6Short
  | Ipv6Long
  | Ipv6Full

type address_object =
  | Ipv6Address
  | Ipv6Network
  | Ipv6Range

module Address : sig
  type t

  val one : t
  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val of_string : string -> t option
  val to_string : t -> ?out_format:format -> string
  val add_int : t -> int -> t
  val sub_int : t -> int -> t
  val add : t -> t -> t
  val sub : t -> t -> t
end

(*
module Range : sig
    (* Implement set semantics *)
    type range_t
    val of_string : string -> range_t option
    val to_string : range_t -> ?out_format:format -> string
end

module Network : sig
    type network_t
    val of_string : string -> network_t option
    val to_string : network_t -> ?out_format:format -> string
    val addresses : 
end
*)