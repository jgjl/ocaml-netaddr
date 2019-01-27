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

module Eui48 :
  sig
    type a
    val bit_size : int
    val zero : a
    val one : a
    val max : a
    val logand : a -> a -> a
    val logor : a -> a -> a
    val logxor : a -> a -> a
    val lognot : a -> a
    val shift_left : a -> int -> a
    val shift_right : a -> int -> a
    val compare : a -> a -> int
    val ( < ) : a -> a -> bool
    val ( > ) : a -> a -> bool
    val add_int : a -> int -> a
    val sub_int : a -> int -> a
    val add : a -> a -> a
    val sub : a -> a -> a
    val get_bit : a -> int -> bool
    val of_string : string -> a option
    val to_string : a -> string
    val to_string_bin : a -> string
    val to_string_oct : a -> string
    val to_string_hex : a -> string
    val of_bin_list : int list -> a option
    val to_bin_list : a -> int list
    val of_int : int -> a
    val to_int : a -> int
    val of_bytes_big_endian : Bytes.t -> int -> a
    val of_std_uint48 : Stdint.uint48 -> a
  end

module IPv4 :
  sig
    type t
    module Address :
      sig
        type a
        val bit_size : int
        val zero : a
        val one : a
        val max : a
        val logand : t -> t -> t
        val logor : t -> t -> t
        val logxor : t -> t -> t
        val lognot : t -> t
        val shift_left : t -> int -> t
        val shift_right : t -> int -> t
        val compare : t -> t -> int
        val ( < ) : a -> a -> bool
        val ( > ) : a -> a -> bool
        val add_int : a -> int -> a
        val sub_int : a -> int -> a
        val add : a -> a -> a
        val sub : a -> a -> a
        val get_bit : a -> int -> bool
        val of_string : string -> a option
        val to_string : a -> string
        val to_string_bin : a -> string
        val to_string_oct : a -> string
        val to_string_hex : a -> string
        val of_bin_list : int list -> a option
        val to_bin_list : a -> int list
        val of_int : int -> a
        val to_int : a -> int
        val of_int32 : int32 -> a
        val to_int32 : a -> int32
        val of_std_uint32 : Stdint.uint32 -> a
        val of_bytes_big_endian : Bytes.t -> int -> a
      end
    module Range :
      sig
        type r
        val make : t -> t -> r option
        val of_string : string -> r option
        val to_string : r -> string
        val get_address : r -> Address.a
        val get_last_address : r -> Address.a
        val size : r -> t
        val contains : r -> Address.a -> bool
        val contains_range : r -> r -> bool
      end
    module Network :
      sig
        type n
        val make : t -> int -> n option
        val of_string : string -> n option
        val to_string : n -> string
        val get_address : n -> Address.a
        val get_last_address : n -> Address.a
        val prefix_len : n -> int
        val contains : n -> Address.a -> bool
        val contains_network : n -> n -> bool
      end
  end

module IPv6 : sig
    type t
    module Address :
      sig
        type a
        val bit_size : int
        val zero : a
        val one : a
        val max : a
        val logand : t -> t -> t
        val logor : t -> t -> t
        val logxor : t -> t -> t
        val lognot : t -> t
        val shift_left : t -> int -> t
        val shift_right : t -> int -> t
        val compare : t -> t -> int
        val ( < ) : a -> a -> bool
        val ( > ) : a -> a -> bool
        val add_int : a -> int -> a
        val sub_int : a -> int -> a
        val add : a -> a -> a
        val sub : a -> a -> a
        val get_bit : a -> int -> bool
        val of_string : string -> a option
        val to_string : a -> string
        val to_string_bin : a -> string
        val to_string_oct : a -> string
        val to_string_hex : a -> string
        val of_bin_list : int list -> a option
        val to_bin_list : a -> int list
        val of_ipv4_address : IPv4.Address.a -> a
        val to_ipv4_address : a -> IPv4.Address.a option
        val of_std_uint128 : Stdint.uint128 -> a
        val of_bytes_big_endian : Bytes.t -> int -> a
      end
    module Range :
      sig
        type r
        val make : Address.a -> Address.a -> r option
        val of_string : string -> r option
        val to_string : r -> string
        val get_address : r -> Address.a
        val get_last_address : r -> Address.a
        val size : r -> t
        val contains : r -> Address.a -> bool
        val contains_range : r -> r -> bool
      end
    module Network :
      sig
        type n
        val make : Address.a -> int -> n option
        val of_string : string -> n option
        val to_string : n -> string
        val get_address : n -> Address.a
        val get_last_address : n -> Address.a
        val prefix_len : n -> int
        val contains : n -> Address.a -> bool
        val contains_network : n -> n -> bool
      end
  end

(*
type ipobject =
  | IPv4Address of IPv4.Address.a
  | IPv6Address of IPv6.Address.a
  | IPv4Range of IPv4.Range.r
  | IPv6Range of IPv6.Range.r
  | IPv4Network of IPv4.Network.n
  | IPv6Network of IPv6.Network.n

val ipobject_of_string : string -> ipobject option
*)