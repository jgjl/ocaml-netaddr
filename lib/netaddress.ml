
exception Result_out_of_range of string
exception Parser_error of string
exception Serialize_error of string

module type NetworkObjectBase = sig
    type t

    val min_str_length : int
    val max_str_length : int

    val serializer : Faraday.t -> t -> unit
    val parser : t Angstrom.t
end


module Parse_helper = struct
    open Angstrom

    let at_most n p =
        if n < 0 then 
            fail "count n < 0"
        else
            let rec loop = function
            | 0 -> return []
            | n -> (lift2 List.cons p (loop (n - 1))) <|> return []
            in
            loop n
    ;;

    let limits n m p =
        (*
            Contributed by seliopou
            https://github.com/inhabitedtype/angstrom/issues/110
        *)
        lift2 (fun xs ys -> xs @ ys)
            (count   n p)
            (at_most m p)
    ;;

    let is_dot =
        function | '.' -> true | _ -> false
    ;;

    let dot =
        skip is_dot
    ;;

    let is_colon =
        function | ':' -> true | _ -> false
    ;;

    let colon =
        skip is_colon
    ;;

    (*
    Parse values in {0..2^n} in decimal format
    *)
    let create_nbit_dec_reader ?strict:(strict=true) bit_size = 
        let max_val = if strict then 
            (1 lsl bit_size) - 1
        else
            1 lsl bit_size 
            in
        let max_dec_digits = int_of_float (ceil (log10 (float_of_int max_val))) in
        let read_nbit_dec = let* n, r = (scan_state (max_dec_digits, 0) 
            (fun (n, v) c -> 
                match n with
                | 0 -> None
                | _ ->
                    match Char.code c with
                    | dec_digit when dec_digit >= 48 && dec_digit <= 57 -> 
                        let new_val = v * 10 + dec_digit - 48 in
                        if new_val <= max_val then
                            Some (n-1, new_val)
                        else
                            None
                    | _ -> None
                )
            ) in
            if n = max_dec_digits then
                fail (Printf.sprintf "Empty %dbit decimal number" bit_size)
            else 
                return r
                    in
        read_nbit_dec
    ;;

    (*
    Parse values in {0..2^n} in hexadecimal format
    *)
    let create_nbyte_hex_reader ?strict:(strict=true) byte_size = 
        let bit_size = byte_size * 8 in
        let max_val = if strict then 
            (1 lsl bit_size) - 1
        else
            1 lsl bit_size
            in
        let max_dec_digits = int_of_float (ceil ((float_of_int bit_size) /. 4.)) in
        let read_nbit_dec = let* n, r = (scan_state (max_dec_digits, 0) 
            (fun (n, v) c -> 
                match n with
                | 0 -> None
                | _ ->
                    match Char.code c with
                    | dec_digit when dec_digit >= 48 && dec_digit <= 57 -> 
                        Some(n-1, v * 16 + dec_digit - 48)
                    | hex_letter_upper when hex_letter_upper >= 65 && hex_letter_upper <= 60 -> 
                        Some(n-1, v * 16 + hex_letter_upper - 65 + 10)
                    | hex_letter_lower when hex_letter_lower >= 97 && hex_letter_lower <= 102 -> 
                        Some(n-1, v * 16 + hex_letter_lower - 97 + 10)
                    | _ -> None
                )
            ) in
            if n = max_dec_digits then
                fail (Printf.sprintf "Empty %dbyte hexadecimal number" byte_size)
            else 
                return r
                    in
        read_nbit_dec
    ;;
end

module Serialize_helper = struct

    let write_hex t value =
        let open Faraday in
        let rec loop v = 
            let c = match (v land 0xf) with
            | 0x0 -> '0' | 0x1 -> '1' | 0x2 -> '2' | 0x3 -> '3'
            | 0x4 -> '4' | 0x5 -> '5' | 0x6 -> '6' | 0x7 -> '7'
            | 0x8 -> '8' | 0x9 -> '9' | 0xa -> 'a' | 0xb -> 'b'
            | 0xc -> 'c' | 0xd -> 'd' | 0xe -> 'e' | 0xf -> 'f'
            | _ -> raise (Serialize_error "Unexpected value.")
                in
            if v > 0xf then
                loop (v lsr 4);
            write_char t c;
            in
        loop value
    ;;
end

module type Address = sig
    type t

    val min_str_length : int
    val max_str_length : int
            
    val bit_size : int
    val zero : t
    val one : t
    val logand : t -> t -> t
    val shift_left : t -> int -> t
    val compare : t -> t -> int
    val parser : t Angstrom.t
    val serializer : Faraday.t -> t -> unit
    val add : t -> t -> t
    val sub : t -> t -> t
    val lognot : t -> t
end

module MakeAddress (N:Stdint.Int) = struct
    let bit_size = N.bits
    let zero = N.zero
    let one = N.one
    let max = N.max_int
    let logand = N.logand
    let logor = N.logor
    let logxor = N.logxor
    let lognot = N.lognot
    let shift_left = N.shift_left
    let shift_right = N.shift_right_logical
    let compare = N.compare
    let to_string_bin = N.to_string_bin
    let to_string_oct = N.to_string_oct
    let to_string_hex = N.to_string_hex

    let add netaddr summand =
        let open N in
        if (max_int - netaddr) < summand then
            raise (Result_out_of_range (Printf.sprintf "%s + %s > %s" (to_string netaddr) (to_string summand) (to_string max_int)))
        else
            netaddr + summand
    ;;

    let sub netaddr subtrahend =
        let open N in
        if netaddr < subtrahend then
            raise (Result_out_of_range (Printf.sprintf "%s - %s < 0" (to_string netaddr) (to_string subtrahend)))
        else
            netaddr - subtrahend
    ;;

    let add_int netaddr summand =
        add netaddr (N.of_int summand)
    ;;

    let sub_int netaddr subtrahend =
        sub netaddr (N.of_int subtrahend)
    ;;

    let get_bit netaddr index =
        let open N in
        (compare (logand (shift_right netaddr Stdlib.(bits - index)) one) one) == 0
    ;;

    let to_bin_list address =
        let rec extract_lsb i value =
        if Stdlib.(i = 0) then
            []
        else
            let next_value = N.shift_right value 1 in
            N.to_int (N.logand N.one value) :: (extract_lsb Stdlib.(i-1) next_value)
        in
        List.rev (extract_lsb N.bits address)
    ;;
    
    let of_bin_list bin_list = 
        if Stdlib.((List.length bin_list) > N.bits) then None
        else
            let two = N.add N.one N.one in
            try Some (List.fold_left (fun a e ->
                let new_acc = N.mul a two in
                let e_value = (match e with 
                | 0 -> N.zero 
                | 1 -> N.one 
                | _ -> raise (Parser_error ("Expected binary value, got " ^ (string_of_int e))))
                    in
                (N.add new_acc e_value)
                ) N.one bin_list)
            with
                Parser_error _ -> None
    ;;
        
    let of_bytes_big_endian = N.of_bytes_big_endian

    let of_int = N.of_int

end


module MakeRange (A:Address) = struct
    type t = {r_first: A.t; r_last: A.t}

    let min_str_length = A.min_str_length + 1 + A.min_str_length
    let max_str_length = A.max_str_length + 1 + A.max_str_length

    let make first last =
        if (A.compare first last) < 0 then
            Some {r_first = first; r_last = last}
        else
            None
    ;;

    let parser =
        let open Angstrom in
        let* rg = lift2 make
            (A.parser <* char '-')
            A.parser 
            in
        match rg with 
        | Some r -> return r 
        | None -> fail "Creating range object failed."
    ;;

    let serializer t range =
        let open Faraday in
        A.serializer t range.r_first; 
        write_char t '-'; 
        A.serializer t range.r_last
    ;;

    let get_address range = 
        range.r_first
    ;;

    let get_last_address range = 
        range.r_last
    ;;

    let size range =
        A.sub range.r_last range.r_first
    ;;

    let contains_address range address =
        if (A.compare range.r_first address) <= 0 &&
            (A.compare range.r_last address) >= 0 then
            true
        else
            false
    ;;

    let contains range1 range2 =
        contains_address range1 range2.r_first && contains_address range1 range2.r_last
    ;;
end

module MakeNetwork (A:Address) = struct
    type t = {n_first: A.t; n_last: A.t; prefix_len: int}

    let min_str_length = A.min_str_length + 1 + 1
    let max_str_length = A.max_str_length + 1 + 3

    let get_address n =
        n.n_first
    ;;

    let get_last_address n =
        n.n_last
    ;;

    let prefix_len network =
        network.prefix_len
    ;;

    let size network = 
        A.sub network.n_last network.n_first

    let make first prefix_len =
        let network_size_bits = A.bit_size - prefix_len in
        let network_mask = A.(shift_left (lognot (shift_left A.one network_size_bits)) prefix_len) in
        let first_masked = A.(logand first network_mask) in
        let network_size = A.(sub (shift_left A.one network_size_bits) A.one) in
        let divisible = A.(logand first network_size) in
        if A.(compare first first_masked) == 0 && A.(compare divisible zero) == 0 then
        begin
            let last = A.(add first network_size) in
            Some {n_first= first; n_last=last; prefix_len= prefix_len}
        end
        else 
            None
    ;;

    let parser =
        let open Angstrom in
        let* nw = lift2 make
            (A.parser <* char '/')
            (Parse_helper.create_nbit_dec_reader ~strict:false A.bit_size)
            in
        match nw with 
        | Some n -> return n 
        | None -> fail "Creating network object failed."
    ;;

    let serializer t network =
        let open Faraday in
        A.serializer t network.n_first; 
        write_char t '/'; 
        write_string t (string_of_int network.prefix_len)
    ;;


    let contains_address network address =
        if (A.compare network.n_first address) <= 0 &&
            (A.compare network.n_last address) >= 0 then
            true
        else
            false
    ;;

    let contains network subnet =
        contains_address network subnet.n_first && contains_address network subnet.n_last
    ;;
end

module MakeFromTo (B:NetworkObjectBase) = struct

    let to_string netaddr =
        let t = Faraday.create B.max_str_length in
        B.serializer t netaddr;
        Faraday.serialize_to_string t
    ;;

    let of_string addr_string =
        let addr_string_len = String.length addr_string in
        if Stdlib.(addr_string_len > B.max_str_length
                    || addr_string_len < B.min_str_length) then
            None
        else
            let complete_parser = Angstrom.(B.parser <* end_of_input) in
            let parsed_addr = Angstrom.parse_string ~consume:All complete_parser addr_string in
            match parsed_addr with
            | Result.Error _ -> None
            | Result.Ok v -> Some v
    ;;

end


(* module MakeNetObjectHelp (O:NetworkObject) = struct
end *)

module Eui48_Address = struct
    open Stdint
    open Angstrom
    open Parse_helper

    type t = uint48
    include MakeAddress(Uint48)

    let min_str_length = 11
    let max_str_length = 17

    let mask_8lsb = Uint48.of_string "0xff"

    let uint_to_hex v = 
        let hex_raw = Uint48.to_string_hex v in
        let hex_raw_len = String.length hex_raw in
        String.sub hex_raw 2 Stdlib.(hex_raw_len-2)
    ;;

    let of_parsed_value b1 b2 b3 b4 b5 b6 =
        let open Uint48 in
        logor (of_int b6)
        (logor (shift_left (of_int b5) 8)
        (logor (shift_left (of_int b4) 16)
        (logor (shift_left (of_int b3) 24)
        (logor (shift_left (of_int b2) 32)
                (shift_left (of_int b1) 40)))))
    ;;

    let parser =
        let read_byte_hex = create_nbyte_hex_reader 1 in
        lift4 
            of_parsed_value
            (read_byte_hex <* colon)
            (read_byte_hex <* colon)
            (read_byte_hex <* colon)
            (read_byte_hex <* colon)
            <*> 
            (read_byte_hex <* colon)
            <*>
            read_byte_hex
    ;;

    let serializer t netaddr =
        let open Uint48 in
        let open Faraday in
        write_string t (uint_to_hex (logand (shift_right netaddr 40) mask_8lsb));
        write_char t ':';
        write_string t (uint_to_hex (logand (shift_right netaddr 32) mask_8lsb));
        write_char t ':';
        write_string t (uint_to_hex (logand (shift_right netaddr 24) mask_8lsb));
        write_char t ':';
        write_string t (uint_to_hex (logand (shift_right netaddr 16) mask_8lsb));
        write_char t ':';
        write_string t (uint_to_hex (logand (shift_right netaddr 8) mask_8lsb));
        write_char t ':';
        write_string t (uint_to_hex (logand netaddr mask_8lsb));
    ;;

    let of_int = Uint48.of_int
    let to_int = Uint48.to_int

    let of_std_uint48 v = v
end

module Eui48_Range = struct
    include MakeRange(Eui48_Address)
end

module Eui48 = struct
    module Address = struct
        include Eui48_Address 
        include MakeFromTo(Eui48_Address)
    end

    type t = Address.t

    module Range = struct
        include Eui48_Range
        include MakeFromTo(Eui48_Range)
    end

    module Infix = struct
        let (<) (a) b = (Address.compare a b) < 0;;
        let (>) a b = (Address.compare a b) > 0;;

        let (+) = Address.add;;
        let (-) = Address.sub;;
        let (+.) = Address.add_int;;
        let (-.) = Address.sub_int;;

        let (@-) = Range.make;;
    end
end


module IPv4_Address = struct
    open Stdint
    open Angstrom
    open Parse_helper

    type t = uint32
    include MakeAddress(Uint32)

    let mask_8lsb = Uint32.of_string "0xff"

    let min_str_length = 7
    let max_str_length = 15

    let of_parsed_value b1 b2 b3 b4 =
        let open Uint32 in
        logor (of_int b4)
            (logor (shift_left (of_int b3) 8)
            (logor (shift_left (of_int b2) 16)
                    (shift_left (of_int b1) 24)))
    ;;

    let parser =
        let read_8bit_dec = create_nbit_dec_reader 8 in
        lift4
            of_parsed_value
            (read_8bit_dec <* dot)
            (read_8bit_dec <* dot)
            (read_8bit_dec <* dot)
            read_8bit_dec
    ;;

    let serializer t netaddr =
        let open Faraday in
        let open Uint32 in
        let b0 = logand netaddr mask_8lsb in
        let b1 = logand (shift_right netaddr 8) mask_8lsb in
        let b2 = logand (shift_right netaddr 16) mask_8lsb in
        let b3 = logand (shift_right netaddr 24) mask_8lsb in
        write_string t (to_string b3);
        write_char t '.';
        write_string t (to_string b2);
        write_char t '.'; 
        write_string t (to_string b1);
        write_char t '.';
        write_string t (to_string b0);
    ;;

    let of_int = Uint32.of_int
    let to_int = Uint32.to_int
    let of_int32 = Uint32.of_int32
    let to_int32 = Uint32.to_int32

    let of_std_uint32 v = v

    let to_16bit_int_list v = 
        let int_v = to_int v in
        let open Stdlib in
        [
            (int_v lsr 16) land 0xffff;
            int_v land 0xffff;
        ]
end

module IPv4_Range = struct
    include MakeRange(IPv4_Address)
end

module IPv4_Network = struct
    include MakeNetwork(IPv4_Address)
end

module IPv4 = struct

    module Address = struct
        include IPv4_Address 
        include MakeFromTo(IPv4_Address)
    end
    type t = Address.t

    module Range = struct
        include IPv4_Range
        include MakeFromTo(IPv4_Range)
    end

    module Network = struct
        include IPv4_Network
        include MakeFromTo(IPv4_Network)
    end

    module Infix = struct
        let (<) (a) b = (Address.compare a b) < 0;;
        let (>) a b = (Address.compare a b) > 0;;

        let (+) = Address.add;;
        let (-) = Address.sub;;
        let (+.) = Address.add_int;;
        let (-.) = Address.sub_int;;

        let (@-) = Range.make;;
        let (@/) = Network.make;;
    end
end

module IPv6_Address = struct
    open Stdint
    open Angstrom
    open Parse_helper

    type t = uint128
    include MakeAddress(Uint128)

    let min_str_length = 2
    let max_str_length = 39

    let mask_16lsb = Uint128.of_string "0xffff"
    let mask_32lsb = Uint128.of_string "0xffffffff"
    let mask_48lsb = Uint128.of_string "0xffffffffffff"
    let mask_third_16lsb = Uint128.of_string "0xffff00000000"


    let ints_to_value list =
        assert (List.length list = 8);
        List.fold_left (fun a e -> 
                            logor (shift_left a 16) (of_int e)
                            ) zero list
    ;;

    let of_parsed_value (part1, part2) : t option =
        let open Stdlib in
        let missing_length = 8 - ((List.length part1) + (List.length part2)) in
        if missing_length = 0 then
            Some (ints_to_value (part1 @ part2))
        else
            let complete_int_list = (List.concat [part1; List.init missing_length (fun _ -> 0); part2]) in
            Some (ints_to_value complete_int_list)
    ;;


    let parser =
        let read_16bit_hex = create_nbyte_hex_reader 2 in
        let* address = lift
                of_parsed_value
                (
                    (* Parse XXXX: parts or : *)
                    let* first_part = lift (fun _ -> []) colon
                                        <|> (at_most 7 (read_16bit_hex <* colon)) in
                    (* Parse :XXXX parts or : *)
                    let+ second_part = match (List.length first_part) with
                                    | 7 -> lift (fun v -> [v]) read_16bit_hex
                                    | 6 -> lift (fun v -> IPv4.Address.to_16bit_int_list v) IPv4.Address.parser
                                            <|> ((limits 1 1 (colon *> read_16bit_hex)) 
                                            <|> lift (fun _ -> []) colon)
                                    | l -> (limits 1 (7-l) (colon *> read_16bit_hex)) 
                                            <|> lift (fun _ -> []) colon 
                                in
                    (first_part, second_part)
                )
            in
        match address with 
        | Some n -> return n 
        | None -> fail "Creating IPv6 address object failed."
    ;;

    let serializer t netaddr = 
        let open Faraday in
        let open Stdlib in
        let rec loop addr_val shift_cnt last_streak_cnt last_max_streak =
            match shift_cnt with
            | 0 ->  (* base case *) 
                last_max_streak 
            | _ ->  (* loop case *)
            begin
                let element = (logand addr_val mask_16lsb) in
                let next_addr_val = (shift_right addr_val 16) in
                let new_streak_cnt, new_max_streak = if element = Uint128.zero then
                                    begin
                                        match last_streak_cnt, last_max_streak with
                                        | None, _ -> Some 0, last_max_streak
                                        | Some lsc, None -> Some (lsc + 1), Some (lsc + 1)
                                        | Some lsc, Some lms -> Some (lsc + 1), Some Stdlib.(max (lsc + 1) lms)
                                    end
                                    else
                                    None, last_max_streak
                    in
                let overall_max_streak = loop next_addr_val (shift_cnt - 1) new_streak_cnt new_max_streak in
                match overall_max_streak, new_streak_cnt with
                | None, Some nsc                  when (nsc > 0) -> 
                    raise (Serialize_error "No max streak but found local streak length > 0.")
                | Some oms, Some nsc when oms = nsc && (nsc > 0) -> 
                    Some (oms - 1)
                | Some oms, Some nsc when oms = nsc && nsc = 0 && shift_cnt = 8 -> 
                    write_char t ':'; 
                    write_char t ':'; 
                    None
                | Some oms, Some nsc when oms = nsc && nsc = 0 -> 
                    write_char t ':'; 
                    None
                | _, _ -> 
                begin
                    if shift_cnt > 1 then
                        write_char t ':';
                    Serialize_helper.write_hex t (Uint128.to_int element);
                    overall_max_streak
                end
            end
            in
        let _ = loop netaddr 8 None None in
        ()
    ;;

    let of_ipv4_address ipv4_address =
        let ipv4_value = Uint128.of_uint32 ipv4_address in
        Uint128.logor mask_third_16lsb ipv4_value
    ;;

    let to_ipv4_address address =
        if Uint128.(compare (logand mask_48lsb address) address) = 0 &&
            Uint128.(compare (logand mask_third_16lsb address) mask_third_16lsb) = 0 then
            Some (Uint128.to_uint32 (Uint128.logand mask_32lsb address))
        else
            None
    ;;
    
    let of_std_uint128 v = v
end

module IPv6_Range = struct
    include MakeRange(IPv6_Address)
end

module IPv6_Network = struct
    include MakeNetwork(IPv6_Address)
end

module IPv6 = struct
    module Address = struct
        include IPv6_Address 
        include MakeFromTo(IPv6_Address)
    end
    type t = Address.t

    module Range = struct
        include IPv6_Range
        include MakeFromTo(IPv6_Range)
    end

    module Network = struct
        include IPv6_Network
        include MakeFromTo(IPv6_Network)
    end

    module Infix = struct
        let (<) (a) b = (Address.compare a b) < 0;;
        let (>) a b = (Address.compare a b) > 0;;

        let (+) = Address.add;;
        let (-) = Address.sub;;
        let (+.) = Address.add_int;;
        let (-.) = Address.sub_int;;

        let (@-) = Range.make;;
        let (@/) = Network.make;;
    end
end

