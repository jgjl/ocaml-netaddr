(*
Todo
 - Networks
 - Ranges
 - Address classification
   https://www.iana.org/assignments/ipv4-address-space/ipv4-address-space.xhtml
   https://www.iana.org/assignments/ipv6-address-space/ipv6-address-space.xhtml
   Create automatic import script? The data is available as XML and csv.
   https://www.iana.org/assignments/ipv4-address-space/ipv4-address-space.csv
   https://www.iana.org/assignments/ipv6-address-space/ipv6-address-space-1.csv

Ideas

 - EUI Addresses
 - IPv4 to IPv6 conversion
*)

exception Result_out_of_range of string
exception Parser_error of string

module IPv4 :
  sig
    type t
    module Address :
      sig
        type a
        val one : a
        val ( < ) : a -> a -> bool
        val ( > ) : a -> a -> bool
        val of_string : string -> a option
        val to_string : a -> string
        val add_int : a -> int -> a
        val sub_int : a -> int -> a
        val add : a -> a -> a
        val sub : a -> a -> a
      end
    module Range :
      sig
        type r
        val of_string : string -> r option
        val to_string : r -> string
        val element_of : r -> Address.a -> bool
        val intersect : r -> r -> bool
        val make : t -> t -> r option
        val size : r -> t
      end
    module Network :
      sig
        type n
        val of_string : string -> n option
        val to_string : n -> string
        val element_of : n -> Address.a -> bool
        val subnet_of : n -> n -> bool
        val prefix_len : n -> int
        val make : t -> int -> n option
      end
  end

module IPv6 : sig
    type t
    module Address :
      sig
        type a
        val one : a
        val ( < ) : a -> a -> bool
        val ( > ) : a -> a -> bool
        val of_string : string -> a option
        val to_string : a -> string
        val of_ipv4_address : IPv4.Address.a -> a
        val to_ipv4_address : a -> IPv4.Address.a option
        val add_int : a -> int -> a
        val sub_int : a -> int -> a
        val add : a -> a -> a
        val sub : a -> a -> a
      end
    module Range :
      sig
        type r
        val of_string : string -> r option
        val to_string : r -> string
        val element_of : r -> Address.a -> bool
        val intersect : r -> r -> bool
        val make : t -> t -> r option
        val size : r -> t
      end
    module Network :
      sig
        type n
        val of_string : string -> n option
        val to_string : n -> string
        val element_of : n -> Address.a -> bool
        val subnet_of : n -> n -> bool
        val prefix_len : n -> int
        val make : t -> int -> n option
      end
  end

type ipaddress =
  | IPv4Address of IPv4.Address.a
  | IPv6Address of IPv6.Address.a
  | IPv4Range of IPv4.Range.r
  | IPv6Range of IPv6.Range.r
  | IPv4Network of IPv4.Network.n
  | IPv6Network of IPv6.Network.n

