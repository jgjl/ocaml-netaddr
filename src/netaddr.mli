
module type NetAddress = sig
  type t
  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val of_string : string -> t option
  val to_string : t -> string
  val opt_to_string : t option -> string
end

module IPv4 : NetAddress
module IPv6 : NetAddress
