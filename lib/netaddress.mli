(*
Todo
 - Networks
 - Ranges
 - Implement official formatting rules: RFC5952
   https://datatracker.ietf.org/doc/html/rfc5952
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

module Parse_helper :
    sig
        val at_most : int -> 'a Angstrom.t -> 'a list Angstrom.t
        val limits : int -> int -> 'a Angstrom.t -> 'a list Angstrom.t
        val read_7bit_dec : int Angstrom.t
        val read_16bit_hex : int Angstrom.t
    end

module Eui48 :
    sig
        type t
        module Address :
        sig
            type t

            val min_str_length : int
            val max_str_length : int

            val bit_size : int
            val zero : t
            val one : t
            val max : t
            val logand : t -> t -> t
            val logor : t -> t -> t
            val logxor : t -> t -> t
            val lognot : t -> t
            val shift_left : t -> int -> t
            val shift_right : t -> int -> t
            val compare : t -> t -> int
            val add_int : t -> int -> t
            val sub_int : t -> int -> t
            val add : t -> t -> t
            val sub : t -> t -> t
            val get_bit : t -> int -> bool
            val to_string_bin : t -> string
            val to_string_oct : t -> string
            val to_string_hex : t -> string
            val of_bin_list : int list -> t option
            val to_bin_list : t -> int list
            val of_int : int -> t
            val to_int : t -> int
            val of_bytes_big_endian : Bytes.t -> int -> t
            val of_std_uint48 : Stdint.uint48 -> t

            val serializer : Faraday.t -> t -> unit
            val parser : t Angstrom.t

            val to_string : t -> string
            val of_string : string -> t option

        end
        module Range :
        sig
            type t

            val min_str_length : int
            val max_str_length : int

            val make : Address.t -> Address.t -> t option

            val serializer : Faraday.t -> t -> unit
            val parser : t Angstrom.t 

            val to_string : t -> string
            val of_string : string -> t option

            val get_address : t -> Address.t
            val get_last_address : t -> Address.t
            val size : t -> Address.t
            val contains_address : t -> Address.t -> bool
            val contains : t -> t -> bool
        end

        module Infix :
        sig
            val (>) : Address.t -> Address.t -> bool
            val (<) : Address.t -> Address.t -> bool
            val (-) : Address.t -> Address.t -> Address.t
            val (+) : Address.t -> Address.t -> Address.t
            val (-.) : Address.t -> int -> Address.t
            val (+.) : Address.t -> int -> Address.t

            val (@-) : Address.t -> Address.t -> Range.t option
        end
    end

module IPv4 :
    sig
        type t
        module Address :
        sig
            type t

            val min_str_length : int
            val max_str_length : int

            val bit_size : int
            val zero : t
            val one : t
            val max : t
            val logand : t -> t -> t
            val logor : t -> t -> t
            val logxor : t -> t -> t
            val lognot : t -> t
            val shift_left : t -> int -> t
            val shift_right : t -> int -> t
            val compare : t -> t -> int
            val add_int : t -> int -> t
            val sub_int : t -> int -> t
            val add : t -> t -> t
            val sub : t -> t -> t
            val get_bit : t -> int -> bool
            val to_string_bin : t -> string
            val to_string_oct : t -> string
            val to_string_hex : t -> string
            val of_bin_list : int list -> t option
            val to_bin_list : t -> int list
            val of_int : int -> t
            val to_int : t -> int
            val of_int32 : int32 -> t
            val to_int32 : t -> int32
            val of_std_uint32 : Stdint.uint32 -> t
            val of_bytes_big_endian : Bytes.t -> int -> t

            val serializer : Faraday.t -> t -> unit
            val parser : t Angstrom.t

            val to_string : t -> string
            val of_string : string -> t option
        end
        module Range :
        sig
            type t

            val min_str_length : int
            val max_str_length : int

            val make : Address.t -> Address.t -> t option

            val serializer : Faraday.t -> t -> unit
            val parser : t Angstrom.t 

            val to_string : t -> string
            val of_string : string -> t option

            val get_address : t -> Address.t
            val get_last_address : t -> Address.t
            val size : t -> Address.t
            val contains_address : t -> Address.t -> bool
            val contains : t -> t -> bool
        end
        module Network :
        sig
            type t

            val min_str_length : int
            val max_str_length : int

            val make : Address.t -> int -> t option

            val serializer : Faraday.t -> t -> unit
            val parser : t Angstrom.t 

            val to_string : t -> string
            val of_string : string -> t option

            val get_address : t -> Address.t
            val get_last_address : t -> Address.t
            val prefix_len : t -> int
            val contains_address : t -> Address.t -> bool
            val contains : t -> t -> bool
        end

        module Infix :
        sig
            val (>) : Address.t -> Address.t -> bool
            val (<) : Address.t -> Address.t -> bool
            val (-) : Address.t -> Address.t -> Address.t
            val (+) : Address.t -> Address.t -> Address.t
            val (-.) : Address.t -> int -> Address.t
            val (+.) : Address.t -> int -> Address.t

            val (@-) : Address.t -> Address.t -> Range.t option
            val (@/) : Address.t -> int -> Network.t option
            (* val (@//) : Network.t -> int -> Network.t list option *)
        end
    end

module IPv6 : sig
    type t
    module Address :
        sig
            type t

            val min_str_length : int
            val max_str_length : int
            val bit_size : int

            val zero : t
            val one : t
            val max : t
            val logand : t -> t -> t
            val logor : t -> t -> t
            val logxor : t -> t -> t
            val lognot : t -> t
            val shift_left : t -> int -> t
            val shift_right : t -> int -> t
            val compare : t -> t -> int
            val add_int : t -> int -> t
            val sub_int : t -> int -> t
            val add : t -> t -> t
            val sub : t -> t -> t
            val get_bit : t -> int -> bool
            val to_string_bin : t -> string
            val to_string_oct : t -> string
            val to_string_hex : t -> string
            val of_bin_list : int list -> t option
            val to_bin_list : t -> int list
            val of_ipv4_address : IPv4.Address.t -> t
            val to_ipv4_address : t -> IPv4.Address.t option
            val of_std_uint128 : Stdint.uint128 -> t
            val of_bytes_big_endian : Bytes.t -> int -> t

            val serializer : Faraday.t -> t -> unit
            val parser : t Angstrom.t

            val to_string : t -> string
            val of_string : string -> t option

        end
    module Range :
        sig
            type t

            val min_str_length : int
            val max_str_length : int

            val make : Address.t -> Address.t -> t option

            val serializer : Faraday.t -> t -> unit
            val parser : t Angstrom.t

            val to_string : t -> string
            val of_string : string -> t option

            val get_address : t -> Address.t
            val get_last_address : t -> Address.t
            val size : t -> Address.t
            val contains : t -> t -> bool
            val contains_address : t -> Address.t -> bool
        end
    module Network :
        sig
            type t

            val min_str_length : int
            val max_str_length : int

            val make : Address.t -> int -> t option

            val serializer : Faraday.t -> t -> unit
            val parser : t Angstrom.t 

            val to_string : t -> string
            val of_string : string -> t option

            val get_address : t -> Address.t
            val get_last_address : t -> Address.t
            val size : t -> Address.t
            val prefix_len : t -> int
            val contains : t -> t -> bool
            val contains_address : t -> Address.t -> bool
        end

    module Infix :
        sig
            val (>) : Address.t -> Address.t -> bool
            val (<) : Address.t -> Address.t -> bool
            val (-) : Address.t -> Address.t -> Address.t
            val (+) : Address.t -> Address.t -> Address.t
            val (-.) : Address.t -> int -> Address.t
            val (+.) : Address.t -> int -> Address.t

            val (@-) : Address.t -> Address.t -> Range.t option
            val (@/) : Address.t -> int -> Network.t option
            (* val (@//) : Network.t -> int -> Network.t list option *)
        end
    end
