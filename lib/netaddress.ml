
exception Result_out_of_range of string
exception Parser_error of string
exception Serialize_error of string

module Parse_helper = struct
    open Angstrom
    open Angstrom.Let_syntax

    let at_most n p =
        if n < 0 
        then fail "count n < 0"
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
    Parse values in {0..2^5} used for prefixes of IPv4 addresses.
    *)
    type read_5bit_dec_state =
    | R5BDS_start
    | R5BDS_r_09
    | R5BDS_r_02
    | R5BDS_end

    let read_5bit_dec =
        (*
            Design reading bytes as state machine.
            Accept is implicitly enabled for all states by the default -> None transition.
        *)
        (scan R5BDS_start 
            (fun (state) c -> 
                match state, c with
                | R5BDS_start, '0'        -> Some R5BDS_end
                | R5BDS_start, '1' .. '2' -> Some R5BDS_r_09
                | R5BDS_start, '3'        -> Some R5BDS_r_02
                | R5BDS_start, '4' .. '9' -> Some R5BDS_end
                | R5BDS_r_09,  '0' .. '9' -> Some R5BDS_end
                | R5BDS_r_02,  '0' .. '2' -> Some R5BDS_end
                | _, _ -> None
                )) >>= function 
                        | _, R5BDS_start -> fail "Could not read 5bit value"
                        | result, _ -> return result 
    ;;

    (*
    Parse values in {0..2^7} used for prefixes of IPv6 addresses.
    *)
    type read_7bit_dec_state =
    | R7BDS_start
    | R7BDS_1
    | R7BDS_1_2
    (* | R7BDS_2_5 *)
    | R7BDS_r_09
    | R7BDS_end

    let read_7bit_dec =
        (*
            Design reading bytes as state machine.
            Accept is implicitly enabled for all states by the default -> None transition.
        *)
        (scan R7BDS_start 
            (fun (state) c -> 
                match state, c with
                | R7BDS_start, '0'        -> Some R7BDS_end
                | R7BDS_start, '1'        -> Some R7BDS_1
                | R7BDS_start, '2' .. '9' -> Some R7BDS_r_09
                | R7BDS_1,     '0' .. '1' -> Some R7BDS_r_09
                | R7BDS_1,     '2'        -> Some R7BDS_1_2
                | R7BDS_1,     '3' .. '9' -> Some R7BDS_end
                | R7BDS_1_2,   '0' .. '8' -> Some R7BDS_end
                | R7BDS_r_09,  '0' .. '9' -> Some R7BDS_end
                | _, _ -> None
                )) >>= function 
                        | _, R7BDS_start -> fail "Could not read 7bit value"
                        | result, _ -> return result 
    ;;

    (*
    Parse values in {0..2^8} used for bytes in the IPv4 dotted quad notation.
    *)
    type read_byte_dec_state =
    | RBDS_start
    | RBDS_01
    | RBDS_2
    | RBDS_39
    | RBDS_2_5
    | RBDS_r_09
    | RBDS_end

    let read_byte_dec =
        (*
            Design reading bytes as state machine.
            Accept is implicitly enabled for all states by the default -> None transition.
        *)
        (scan RBDS_start 
            (fun (state) c -> 
                match state, c with
                | RBDS_start, '0' .. '1' -> Some RBDS_01
                | RBDS_start, '2'        -> Some RBDS_2
                | RBDS_start, '3' .. '9' -> Some RBDS_39
                | RBDS_01,    '0' .. '9' -> Some RBDS_r_09
                | RBDS_r_09,  '0' .. '9' -> Some RBDS_end
                | RBDS_39,    '0' .. '9' -> Some RBDS_end
                | RBDS_2,     '0' .. '4' -> Some RBDS_r_09
                | RBDS_2,     '5'        -> Some RBDS_2_5
                | RBDS_2,     '6' .. '9' -> Some RBDS_end
                | RBDS_2_5,   '0' .. '5' -> Some RBDS_end
                | _, _ -> None
                )) >>= function 
                        | _, RBDS_start -> fail "Could not read byte value"
                        | result, _ -> return result 
    ;;

    (*
    Parse values in {0..2^8} used for bytes in the EUI coloned hexa notation.
    *)
    type read_byte_hex_state =
    | RBHS_start
    | RBHS_first
    | RBHS_end

    let read_byte_hex =
        (scan RBHS_start 
            (fun (state) c -> 
                match state, c with
                | RBHS_start, '0' .. 'f' -> Some RBHS_first
                | RBHS_first, '0' .. 'f' -> Some RBHS_end
                | _, _ -> None
                )) >>= function 
                        | _, RBHS_start -> fail "Could not read byte value"
                        | result, _ -> return result 
    ;;

    (*
    Parse values in {0..2^16} used for bytes in the IPv6-coloned hex notation.
    *)
    let read_16bit_hex = 
        let* n, r = (scan_state (4, 0) 
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
        match n with
        | 4 -> fail "Empty hex number"
        | _ -> return r
    ;;
end

module Serialize_helper = struct

    let write_hex t value =
        let open Faraday in
        let rec loop v = 
            let c = match (v land 0xf) with
            | 0x0 -> '0'
            | 0x1 -> '1'
            | 0x2 -> '2'
            | 0x3 -> '3'
            | 0x4 -> '4'
            | 0x5 -> '5'
            | 0x6 -> '6'
            | 0x7 -> '7'
            | 0x8 -> '8'
            | 0x9 -> '9'
            | 0xa -> 'a'
            | 0xb -> 'b'
            | 0xc -> 'c'
            | 0xd -> 'd'
            | 0xe -> 'e'
            | 0xf -> 'f'
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
    type a

    val bit_size : int
    val zero : a
    val one : a
    val logand : a -> a -> a
    val shift_left : a -> int -> a
    val compare : a -> a -> int
    val ( < ) : a -> a -> bool
    val to_string : a -> string
    val serialize : Faraday.t -> a -> unit
    val add : a -> a -> a
    val sub : a -> a -> a
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
    let (<) (a) b =
        (N.compare a b) < 0
    let (>) a b =
        (N.compare a b) > 0

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
end

module MakeRange (A:Address) = struct
    type r = {r_first: A.a; r_last: A.a}

    let make first last =
        if A.(first < last) then
            Some {r_first = first; r_last = last}
        else
            None
    ;;

    let serialize t range =
        let open Faraday in
        A.serialize t range.r_first; 
        write_char t '-'; 
        A.serialize t range.r_last
    ;;

    let to_string range =
        (A.to_string range.r_first) ^ "-" ^ (A.to_string range.r_last)
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
    type n = {n_first: A.a; n_last: A.a; prefix_len: int}

    let get_address n =
        n.n_first
    ;;

    let get_last_address n =
        n.n_last
    ;;

    let to_string network =
        A.to_string network.n_first ^ "/" ^ string_of_int network.prefix_len
    ;;

    let serialize t network =
        let open Faraday in
        A.serialize t network.n_first; 
        write_char t '/'; 
        write_string t (string_of_int network.prefix_len)
    ;;

    let prefix_len network =
        network.prefix_len
    ;;

    let make first prefix_len =
        let network_size_bits = A.bit_size - prefix_len in
        let network_size = A.(sub (shift_left A.one network_size_bits) A.one) in
        let last = A.(add first network_size) in
        let divisible = A.(logand first network_size) in
        if A.(compare divisible zero) == 0 then
            Some {n_first= first; n_last=last; prefix_len= prefix_len}
        else 
            None
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


module Eui48 = struct
    open Stdint
    open Angstrom
    open Parse_helper

    let mask_8lsb = Uint48.of_string "0xff"

    type a = uint48

    include MakeAddress(Uint48)

    let min_str_length_address = 11
    let max_str_length_address = 17

    let parser_eui48_part =
        lift4 (fun b1 b2 b3 b4 b5 b6 -> 
                (int_of_string b1, 
                int_of_string b2, 
                int_of_string b3, 
                int_of_string b4, 
                int_of_string b5, 
                int_of_string b6))
        (read_byte_hex <* colon)
        (read_byte_hex <* colon)
        (read_byte_hex <* colon)
        (read_byte_hex <* colon)
        <*> 
        (read_byte_hex <* colon)
        <*>
        read_byte_hex
    ;;

    let parser_address = 
        parser_eui48_part <* end_of_input
    ;;

    let uint_to_hex v = 
        let hex_raw = Uint48.to_string_hex v in
        let hex_raw_len = String.length hex_raw in
        String.sub hex_raw 2 Stdlib.(hex_raw_len-2)
    ;;

    let of_strings b1 b2 b3 b4 b5 b6 =
        (int_of_string b6) lor
        (((int_of_string b5) lsl 8) lor
        (((int_of_string b4) lsl 16) lor
        (((int_of_string b3) lsl 24) lor
        (((int_of_string b2) lsl 32) lor
        ((int_of_string b1) lsl 40)))))
    ;;

    let of_parsed_value (b1, b2, b3, b4, b5, b6) =
        let open Uint48 in
        logor (of_int b6)
        (logor (shift_left (of_int b5) 8)
        (logor (shift_left (of_int b4) 16)
        (logor (shift_left (of_int b3) 24)
        (logor (shift_left (of_int b2) 32)
                (shift_left (of_int b1) 40)))))
    ;;

    let of_string s =
        let open Stdlib in
        if (String.length s) > max_str_length_address
        || (String.length s) < min_str_length_address then
            None
        else
            match (Angstrom.parse_string ~consume:All parser_address s) with
            | Result.Ok result -> Some (of_parsed_value result)
            | _ -> None
    ;;

    let serialize t netaddr =
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

    let to_string netaddr =
        let t = Faraday.create max_str_length_address in
        serialize t netaddr;
        Faraday.serialize_to_string t
    ;;

    let of_int = Uint48.of_int
    let to_int = Uint48.to_int

    let of_std_uint48 v = v
end


module IPv4 = struct
    open Stdint

    let mask_8lsb = Uint32.of_string "0xff"

    type t = uint32

    module Parser = struct
        open Angstrom
        open Parse_helper

        let min_str_length_address = 7
        let min_str_length_range = 15
        let min_str_length_network = 9

        let max_str_length_address = 15
        let max_str_length_range = 31
        let max_str_length_network = 18

        (* let stringbytes_to_list b1 b2 b3 b4 =
            [int_of_string b1; int_of_string b2; int_of_string b3; int_of_string b4]
        ;; *)

        let parser_ipv4_part =
            (lift4
            (fun b1 b2 b3 b4 -> int_of_string b1, int_of_string b2, int_of_string b3, int_of_string b4)
            (read_byte_dec <* dot)
            (read_byte_dec <* dot)
            (read_byte_dec <* dot)
            read_byte_dec)
        ;;

        let parser_address = 
            parser_ipv4_part <* end_of_input
        ;;

        let parser_range =
            lift2 (fun first_value last_value -> (first_value, last_value))
            (parser_ipv4_part <* char '-')
            (parser_ipv4_part <* end_of_input)
        ;;

        let parser_network =
            lift2 (fun network_value prefix_len -> (network_value, int_of_string prefix_len))
            (parser_ipv4_part <* char '/')
            (read_5bit_dec <* end_of_input)
    end

    module Address = struct
        type a = t
        include MakeAddress(Uint32)

        let of_parsed_value (b1, b2, b3, b4) =
            let open Uint32 in
            logor (of_int b4)
                (logor (shift_left (of_int b3) 8)
                (logor (shift_left (of_int b2) 16)
                        (shift_left (of_int b1) 24)))
        ;;

        let of_strings b1 b2 b3 b4 =
            ((int_of_string b4) lor
            (((int_of_string b3) lsl 8) lor
            (((int_of_string b2) lsl 16) lor
            ((int_of_string b1) lsl 24))))
        ;;

        let of_string s =
            let open Stdlib in
            if (String.length s) > Parser.max_str_length_address
            || (String.length s) < Parser.min_str_length_address then
            None
            else
            match Angstrom.parse_string ~consume:All Parser.parser_address s with
            | Result.Ok result -> Some (of_parsed_value result)
            | _ -> None
        ;;

        let serialize t netaddr =
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

        let to_string netaddr =
            let t = Faraday.create Parser.max_str_length_address in
            serialize t netaddr;
            Faraday.serialize_to_string t
        ;;

        let of_int = Uint32.of_int
        let to_int = Uint32.to_int
        let of_int32 = Uint32.of_int32
        let to_int32 = Uint32.to_int32

        let of_std_uint32 v = v
    end

    module Range = struct
        include MakeRange(Address)

        let of_string s =
            if Stdlib.((String.length s) > Parser.max_str_length_range
                        || (String.length s) < Parser.min_str_length_range) then
                None
            else
                match Angstrom.parse_string  ~consume:All Parser.parser_range s with
                | Result.Ok (first_value, last_value) ->
                make (Address.of_parsed_value first_value) (Address.of_parsed_value last_value)
                | _ -> None
        ;;
    end

    module Network = struct
        include MakeNetwork(Address)

        let of_string network_string =
            let network_string_len = String.length network_string in
            if Stdlib.(network_string_len > Parser.max_str_length_network
                        || network_string_len < Parser.min_str_length_network) then
                None
            else
                let network_range = Angstrom.parse_string ~consume:All Parser.parser_network network_string in
                match network_range with
                | Result.Error _ -> None
                | Result.Ok (parsed_network_address, prefix_len) ->
                let network_address = Address.of_parsed_value parsed_network_address in
                make network_address prefix_len
        ;;
    end
end

module IPv6 = struct
    open Stdint

    type t = uint128

    let mask_16lsb = Uint128.of_string "0xffff"
    let mask_32lsb = Uint128.of_string "0xffffffff"
    let mask_48lsb = Uint128.of_string "0xffffffffffff"
    let mask_third_16lsb = Uint128.of_string "0xffff00000000"

    module Parser = struct    
        open Angstrom
        open Parse_helper

        let min_str_length_address = 2
        let min_str_length_range = 5
        let min_str_length_network = 4

        let max_str_length_address = 39
        let max_str_length_range = 79
        let max_str_length_network = 43

        let parser_value_part =
            let ipv4_part_to_16bit_list = function
                | (b1, b2, b3, b4) -> [b1 * 256 + b2; b3 * 256 + b4]
                in
            (* Parse XXXX: parts or : *)
            let* first_part = lift (fun _ -> []) colon
                              <|> (at_most 7 (read_16bit_hex <* colon)) in
            (* Parse :XXXX parts or : *)
            let+ second_part = match (List.length first_part) with
                          | 7 -> lift (fun v -> [v]) read_16bit_hex
                          | 6 -> lift (fun v -> ipv4_part_to_16bit_list v) IPv4.Parser.parser_ipv4_part
                                 <|> ((limits 1 1 (colon *> read_16bit_hex)) 
                                 <|> lift (fun _ -> []) colon)
                          | l -> (limits 1 (7-l) (colon *> read_16bit_hex)) 
                                 <|> lift (fun _ -> []) colon 
                        in
            (first_part, second_part)
        ;;

        let parser_address =
            lift (fun (part1, part2) -> (part1, part2))
                (parser_value_part <* end_of_input)
        ;;

        let parser_range =
            lift2 (fun (s_p1, s_p2) (e_p1, e_p2) ->
                    (s_p1, s_p2),
                    (e_p1, e_p2))
            (parser_value_part <* char '-')
            (parser_value_part <* end_of_input)
        ;;

        let parser_network =
            lift2 (fun (s_p1, s_p2) prefix_len ->
                    (s_p1, s_p2), (int_of_string prefix_len))
            (parser_value_part <* char '/')
            (read_7bit_dec <* end_of_input)
        ;;
    end

    module Address = struct
        type a = t

        include MakeAddress(Uint128)

        let ints_to_value list =
            assert (List.length list = 8);
            List.fold_left (fun a e -> 
                                Uint128.(
                                logor (shift_left a 16) (of_int e)
                                )) Uint128.zero list
        ;;

        let of_parsed_value (part1, part2) =
            let open Stdlib in
            let missing_length = 8 - ((List.length part1) + (List.length part2)) in
            if missing_length = 0 then
                Some (ints_to_value (part1 @ part2))
            else
                let complete_int_list = (List.concat [part1; List.init missing_length (fun _ -> 0); part2]) in
                Some (ints_to_value complete_int_list)
        ;;

        let of_string ipv6_string =
            let ipv6_string_len = String.length ipv6_string in
            if Stdlib.(ipv6_string_len > Parser.max_str_length_address 
                        || ipv6_string_len < Parser.min_str_length_address) then
                None
            else
                let parsed_ipv6 = Angstrom.parse_string ~consume:All Parser.parser_address ipv6_string in
                match parsed_ipv6 with
                | Result.Error _ -> None
                | Result.Ok (part1, part2) -> of_parsed_value (part1, part2)
        ;;

        let serialize t netaddr = 
            let open Faraday in
            let open Stdlib in
            let rec loop addr_val shift_cnt last_streak_cnt last_max_streak =
                match shift_cnt with
                | 0 ->  (* base case *) 
                    last_max_streak 
                | _ ->  (* loop case *)
                begin
                    let element = Uint128.(logand addr_val mask_16lsb) in
                    let next_addr_val = Uint128.(shift_right addr_val 16) in
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
        
        let to_string netaddr =
            let t = Faraday.create Parser.max_str_length_address in
            serialize t netaddr;
            Faraday.serialize_to_string t
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

    module Range = struct
        include MakeRange(Address)

        let of_string range_string =
            let range_string_len = String.length range_string in
            if Stdlib.(range_string_len > Parser.max_str_length_range 
                        || range_string_len < Parser.min_str_length_range) then
                None
            else
                let parsed_range = Angstrom.parse_string ~consume:All Parser.parser_range range_string in
                match parsed_range with
                | Result.Error _ -> None
                | Result.Ok (first_parsed,last_parser) ->
                let first = Address.of_parsed_value first_parsed in
                let last = Address.of_parsed_value last_parser in
                match first, last with
                | Some first_address, Some last_address ->
                    make first_address last_address
                | _, _ -> None
        ;;
    end

    module Network = struct
        include MakeNetwork(Address)

        let of_string network_string =
            let network_string_len = String.length network_string in
            if Stdlib.(network_string_len > Parser.max_str_length_network 
                        || network_string_len < Parser.min_str_length_network) then
                        (
                            Printf.printf "ERROR: string of wrong size"; None
                        )
            else
                let network_range = Angstrom.parse_string ~consume:All Parser.parser_network network_string in
                match network_range with
                | Result.Error e -> Printf.printf "ERROR: %s" e; None
                | Result.Ok (parsed_network_address, prefix_len) ->
                    let network_address_opt = Address.of_parsed_value parsed_network_address in
                    match network_address_opt with
                    | None -> None
                    | Some network_address -> 
                        make network_address prefix_len
        ;;
    end
end

