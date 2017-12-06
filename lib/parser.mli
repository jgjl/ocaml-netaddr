
val read_byte : string Angstrom.t

type parsed_ipv4 = int * int * int * int
(*
 * Get a parser for IPv4 dotted quad notation
 *)
val parser_ipv4 : parsed_ipv4 Angstrom.t

(*
 * Apply the parser to the input string and return the result as a string
 *)
val parse_ipv4 : string -> string

(*
type parsed_ipv6 =
    | ParsedIPv6Complete of int list
    | ParsedIpv6TwoParts of int list * int list
    *)
type parsed_ipv6 = int list * int list
(*
 * Get a parser for IPv4 dotted quad notation
 *)
val parser_ipv6 : parsed_ipv6 Angstrom.t

(*
 * Apply the parser to the input string and return the result as a string
 *)
val parse_ipv6 : string -> string