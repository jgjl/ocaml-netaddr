
(*
 * Get a parser for IPv4 dotted quad notation
 *)
val parser_ipv4 : int list Angstrom.t

(*
 * Apply the parser to the input string and return the result as a string
 *)
val parse_ipv4 : string -> string

(*
(*
 * Get a parser for IPv4 dotted quad notation
 *)
val parser_ipv6 : int list Angstrom.t

(*
 * Apply the parser to the input string and return the result as a string
 *)
val parse_ipv6 : string -> string
*)