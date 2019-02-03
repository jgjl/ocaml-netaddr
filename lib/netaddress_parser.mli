
(* Read 8, 7, or 5 bit values in decimal or hex encoding *)
val read_5bit_dec : string Angstrom.t
val read_7bit_dec : string Angstrom.t
val read_byte_dec : string Angstrom.t
val read_byte_hex : string Angstrom.t
val read_16bit_hex : string Angstrom.t
val read_16bit_hex_1 : string Angstrom.t
val read_16bit_hex_2 : string Angstrom.t
val read_16bit_hex_3 : int Angstrom.t

module Eui48 : sig
    type parsed_value = int * int * int * int * int * int
    type parsed_value_prefix = int

    val min_str_length_address : int

    val max_str_length_address : int
    (*
     * Get a parser for IPv4 dotted quad notation
     *)
    val parser_address : parsed_value Angstrom.t
end

module IPv4 : sig
    type parsed_value = int * int * int * int
    type parsed_value_prefix = int

    val min_str_length_address : int
    val min_str_length_range : int
    val min_str_length_network : int

    val max_str_length_address : int
    val max_str_length_range : int
    val max_str_length_network : int
    (*
     * Get a parser for IPv4 dotted quad notation
     *)
    val parser_address : parsed_value Angstrom.t
    val parser_range : (parsed_value * parsed_value) Angstrom.t
    val parser_network : (parsed_value * parsed_value_prefix) Angstrom.t
end

module IPv6 : sig
    type parsed_value = int list * int list
    type parsed_value_prefix = int

    val min_str_length_address : int
    val min_str_length_range : int
    val min_str_length_network : int

    val max_str_length_address : int
    val max_str_length_range : int
    val max_str_length_network : int

    val parser_value_part_1 : (string list * string list) Angstrom.t
    val parser_value_part_2 : (string list * string list) Angstrom.t
    (*
     * Get a parser for IPv6 coloned octuple notation
     *)
    val parser_address : parsed_value Angstrom.t
    val parser_range : (parsed_value * parsed_value) Angstrom.t
    val parser_network : (parsed_value * parsed_value_prefix) Angstrom.t
end

