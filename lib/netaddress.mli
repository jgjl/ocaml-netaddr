
  (* 
    Todo
     - Networks
     - Ranges
     - Address classification 
       https://www.iana.org/assignments/ipv4-address-space/ipv4-address-space.xhtml
       https://www.iana.org/assignments/ipv6-address-space/ipv6-address-space.xhtml
       Create automatic import script? The data is available as XML and csv.

    Ideas

     - EUI Addresses
     - IPv4 to IPv6 conversion
  *)

exception Result_out_of_range of string

module type NetAddress = sig
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

module type IPv4 = sig
  type t
  include NetAddress with type t := t
end

module type IPv6 = sig
  type t
  include NetAddress with type t := t
  (*
  val of_ipv4 : IPv4.t -> t
  *)
end

(*
type ipaddress =
  | IPv4Address of IPv4.t
  | IPv6Address of IPv6.t
  *)