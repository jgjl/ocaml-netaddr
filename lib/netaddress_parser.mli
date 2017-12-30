
val read_byte : string Angstrom.t
val read_bit7 : string Angstrom.t
val read_bit5 : string Angstrom.t

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
    (*
     * Get a parser for IPv6 coloned octuple notation
     *)
    val parser_address : parsed_value Angstrom.t
    val parser_range : (parsed_value * parsed_value) Angstrom.t
    val parser_network : (parsed_value * parsed_value_prefix) Angstrom.t
end

