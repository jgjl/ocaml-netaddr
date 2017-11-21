
(*
    Text representation of IPv6 addresses
    https://tools.ietf.org/html/rfc4291
*)

type t
val ( < ) : t -> t -> bool
val ( > ) : t -> t -> bool
val one : t
val of_string : string -> t option
val to_string : t -> string
val add_int : t -> int -> t
val sub_int : t -> int -> t
val add : t -> t -> t
val sub : t -> t -> t
