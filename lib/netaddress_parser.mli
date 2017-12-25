
val read_byte : string Angstrom.t
val read_bit7 : string Angstrom.t
val read_bit5 : string Angstrom.t

type parsed_ipv4 = int * int * int * int

type parsed_ipv4_prefix = int
(*
 * Get a parser for IPv4 dotted quad notation
 *)
val parser_ipv4 : parsed_ipv4 Angstrom.t
val parser_ipv4_range : (parsed_ipv4 * parsed_ipv4) Angstrom.t
val parser_ipv4_network : (parsed_ipv4 * parsed_ipv4_prefix) Angstrom.t

type parsed_ipv6 = int list * int list
type parsed_ipv6_prefix = int
(*
 * Get a parser for IPv6 dotted quad notation
 *)
val parser_ipv6 : parsed_ipv6 Angstrom.t
val parser_ipv6_range : (parsed_ipv6 * parsed_ipv6) Angstrom.t
val parser_ipv6_network : (parsed_ipv6 * parsed_ipv6_prefix) Angstrom.t

