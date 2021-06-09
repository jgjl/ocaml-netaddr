
exception Result_out_of_range of string
exception Parser_error of string

module Parse_helper = struct
    open Angstrom

    type parse_result = 
    | Some_result
    | No_result

    let rec at_most m p =
        (*
            Contributed by seliopou
            https://github.com/inhabitedtype/angstrom/issues/110
        *)
        match m with
        | 0 -> return []
        | _ -> (lift2 (fun x xs -> x :: xs) p (at_most (m - 1) p))
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

    let rec at_most_split c m pf pr s f =
        (*
            m = max. count
            pf = first part to match
            pr = rest part to match
            s = split part
            f = split part for the first read field
        *)
        if c = m-1 then
            (lift (fun x -> ([x], [])) s)
        else 
            (* Parse (XXXX:)* parts *)
            (lift2 (fun x1 (xs1, xs2) -> (x1 :: xs1, xs2)) pf (at_most_split (c+1) m pf pr s f))
            <|>
            (* Parse (:XXXX) parts, special case handling for addresses starting with "::" *)
            (
                if c = 0 then (
                    lift2 (fun _ xs2 -> ([], xs2)) f (limits 1 6 pr)
                    <|>
                    (lift2 (fun _ _ -> ([], [])) f f)
                    )
                else (
                    (*(lift2 (fun x2 xs2 -> ([], x2::xs2)) s (limits 1 (m-c-1) pr))*)
                    (lift (fun xs -> ([], xs)) (limits 1 (m-c-1) pr))
                    <|>
                    (lift (fun _ -> ([], [])) f)
                )
            )
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

    let is_hexdigit = 
        function 
        | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true 
        | _ -> false
    ;;

    let hexdigit = 
        satisfy is_hexdigit
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
    Parse values in {0..2^16} used for bytes in the IPv6 coloned hexa notation.
    *)
    let read_16bit_hex_1 =
        lift2 (fun f r -> (String.make 1 f) ^ r)
            hexdigit
            (scan_string 0 (fun pos c -> if (is_hexdigit c) && pos < 3 then
                                Some (pos + 1)
                                else
                                None))
    ;;


    let hex_char_to_int c = 
        match Char.code c with
        | dec_digit when dec_digit >= 48 && dec_digit <= 57 -> dec_digit - 48
        | hex_letter_upper when hex_letter_upper >= 65 && hex_letter_upper <= 60 -> hex_letter_upper - 65 + 10
        | hex_letter_lower when hex_letter_lower >= 97 && hex_letter_lower <= 102 -> hex_letter_lower - 97 + 10
        | _ -> raise (Parser_error ("Expected hex value, got " ^ (String.make 1 c)))

    (*
    Parse values in {0..2^16} used for bytes in the IPv6 coloned hexa notation.
    *)
    let read_16bit_hex_1_1 =
        lift2 (fun f (_, r) -> (hex_char_to_int f)* r)
            hexdigit
            (scan_state (0, 0) (fun (pos, rv) c -> if (is_hexdigit c) && pos < 3 then
                                Some (pos + 1, rv * 16 + hex_char_to_int c)
                                else
                                None))
    ;;

    let read_16bit_hex_2 = 
        (scan_string 4 
            (fun state c -> 
                match state, (is_hexdigit c) with
                | 4, true -> Some 3
                | 3, true -> Some 2
                | 2, true -> Some 1
                | 1, true -> Some 0
                | _, _ -> None
                )
            )
    ;;

    let read_16bit_hex_5 = 
        lift2 (fun f r -> Char.escaped f ^ r)
            hexdigit
            (scan_string 1 
                (fun state c -> 
                    match state, (is_hexdigit c) with
                    | 3, true -> Some 4
                    | 2, true -> Some 3
                    | 1, true -> Some 2
                    | _, _ -> None
                    )
                )
    ;;

    let rec read_16bit_hex_6 r n = 
        hexdigit >>=
            fun h ->
            begin 
                let rn = (r lsl 4) + (int_of_char h) - 48 in
                if n > 0 then
                    read_16bit_hex_6 rn (n - 1)
                    <|>
                    return rn
                else
                    return rn
            end
    ;;

    let read_16bit_hex_4 = 
        (scan_string 4 
            (fun n c -> 
                if n < 1 then
                    None
                else 
                    match c with
                    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> Some (n - 1)
                    | _ -> None
                )
        )
    ;;

    let read_16bit_hex_7 = 
        (scan_string 4 
            (fun n c -> 
                match n with
                | 0 -> None
                | _ ->
                    match c with
                    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> Some (n - 1)
                    | _ -> None
                )
        )
    ;;

    let read_16bit_hex_7_1 = 
        lift (fun (_, r) -> r)
        (scan_state (4, 0) 
            (fun (n, v) c -> 
                match n with
                | 0 -> None
                | _ ->
                    match Char.code c with
                    | dec_digit when dec_digit >= 48 && dec_digit <= 57 -> Some(n-1, v * 16 + dec_digit - 48)
                    | hex_letter_upper when hex_letter_upper >= 65 && hex_letter_upper <= 60 -> Some(n-1, v * 16 + hex_letter_upper - 65 + 10)
                    | hex_letter_lower when hex_letter_lower >= 97 && hex_letter_lower <= 102 -> Some(n-1, v * 16 + hex_letter_lower - 97 + 10)
                    | _ -> None
                )
        )
    ;;

    let read_16bit_hex_3 = 
        lift (fun cl -> List.fold_left (fun r c -> (r lsl 4) + (int_of_char c)) 0 cl) (at_most 4 hexdigit)
    ;;

    let read_16bit_hex_8 =
        let rec read_hex r v = 
            match r with
            | 0 -> return v
            | r -> lift (fun d -> (v lsl 4) + (Char.code d) - 48) hexdigit >>= read_hex (r - 1)
            in
        read_hex 4 0
    ;;

    (* let rec read_16bit_hex_9 =
        let rec read_hex r v = 
            match r with
            | 0 -> return v
            | r -> (lift2 (fun x xr -> (v lsl 4) + (Char.code d) - 48) hexdigit (read_hex (r - 1)))
    ;; *)

    let read_16bit_hex = read_16bit_hex_7
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

    let to_string range =
        (A.to_string range.r_first) ^ "-" ^ (A.to_string range.r_last)
    ;;

    let serialize t range =
        let open Faraday in
        A.serialize t range.r_first; 
        write_char t '-'; 
        A.serialize t range.r_last
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

    let contains range address =
        if (A.compare range.r_first address) <= 0 &&
            (A.compare range.r_last address) >= 0 then
            true
        else
            false
    ;;

    let contains_range range1 range2 =
        contains range1 range2.r_first && contains range1 range2.r_last
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

    let contains network address =
        if (A.compare network.n_first address) <= 0 &&
            (A.compare network.n_last address) >= 0 then
            true
        else
            false
    ;;

    let contains_network network subnet =
        contains network subnet.n_first && contains network subnet.n_last
    ;;
end


module Eui48 = struct
    open Stdint
    open Angstrom
    open Parse_helper

    let mask_8lsb = Uint48.of_string "0xff"

    type a = uint48

    include MakeAddress(Uint48)

    type parsed_value = int * int * int * int * int * int

    let min_str_length_address = 11

    let max_str_length_address = 17

    (* let stringbytes_to_list b1 b2 b3 b4 b5 b6 =
        [int_of_string b1; int_of_string b2; int_of_string b3; int_of_string b4; int_of_string b5; int_of_string b6]
    ;; *)

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

    let to_string netaddr =
        let open Uint48 in
        let b1 = logand netaddr mask_8lsb in
        let b2 = logand (shift_right netaddr 8) mask_8lsb in
        let b3 = logand (shift_right netaddr 16) mask_8lsb in
        let b4 = logand (shift_right netaddr 24) mask_8lsb in
        let b5 = logand (shift_right netaddr 32) mask_8lsb in
        let b6 = logand (shift_right netaddr 40) mask_8lsb in
        (uint_to_hex b6 ^":"^ uint_to_hex b5 
                        ^":"^ uint_to_hex b4 
                        ^":"^ uint_to_hex b3 
                        ^":"^ uint_to_hex b2 
                        ^":"^ uint_to_hex b1)
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

        type parsed_value = int * int * int * int
        type parsed_value_prefix = Prefix_value of int [@@unbox]

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

        let parser_ipv4_part_2 a_make_fun =
            (lift4
            a_make_fun
            (read_byte_dec <* dot)
            (read_byte_dec <* dot)
            (read_byte_dec <* dot)
            read_byte_dec)
        ;;

        let parser_address = 
            parser_ipv4_part <* end_of_input
        ;;

        let parser_address_2 a_make_fun = 
            (parser_ipv4_part_2 a_make_fun) <* end_of_input
        ;;

        let parser_range =
            lift2 (fun first_value last_value -> (first_value, last_value))
            (parser_ipv4_part <* char '-')
            (parser_ipv4_part <* end_of_input)
        ;;

        let parser_range_2 a_make_fun r_make_fun =
            lift2 r_make_fun
            ((parser_ipv4_part_2 a_make_fun) <* char '-')
            ((parser_ipv4_part_2 a_make_fun) <* end_of_input)
        ;;

        let parser_network =
            lift2 (fun network_value prefix_len -> (network_value, int_of_string prefix_len))
            (parser_ipv4_part <* char '/')
            (read_5bit_dec <* end_of_input)

        let parser_network_2 a_make_fun n_make_fun =
            lift2 n_make_fun
            ((parser_ipv4_part_2 a_make_fun) <* char '/')
            (read_5bit_dec <* end_of_input)
        ;;
    end

    module Address = struct
        open Parse_helper

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

        let to_string netaddr =
            let open Uint32 in
            let b0 = logand netaddr mask_8lsb in
            let b1 = logand (shift_right netaddr 8) mask_8lsb in
            let b2 = logand (shift_right netaddr 16) mask_8lsb in
            let b3 = logand (shift_right netaddr 24) mask_8lsb in
            (to_string b3 ^"."^ to_string b2 
                          ^"."^ to_string b1 
                          ^"."^ to_string b0)
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

    let shift_list = [112;96;80;64;48;32;16;0]

    type cur_streak = { cur_streak_start : int; cur_streak_len : int}
    type max_streak = { max_streak_start : int; max_streak_len : int}
    type streak = { streak_start : int; streak_len : int}

    let mask_16lsb = Uint128.of_string "0xffff"
    let mask_32lsb = Uint128.of_string "0xffffffff"
    let mask_48lsb = Uint128.of_string "0xffffffffffff"
    let mask_third_16lsb = Uint128.of_string "0xffff00000000"

    let max_streak_of_cur_streak cur_streak =
        {max_streak_start = cur_streak.cur_streak_start; 
        max_streak_len = cur_streak.cur_streak_len}
    ;;

    let streak_of_cur_streak cur_streak =
        {streak_start = cur_streak.cur_streak_start; 
        streak_len = cur_streak.cur_streak_len}
    ;;

    let streak_of_max_streak max_streak =
        {streak_start = max_streak.max_streak_start; 
        streak_len = max_streak.max_streak_len}
    ;;

    module Parser = struct    
        open Angstrom
        open Parse_helper

        type parsed_value = int list * int list
        type parsed_value_prefix = Prefix_value of int [@@unbox]

        let min_str_length_address = 2
        let min_str_length_range = 5
        let min_str_length_network = 4

        let max_str_length_address = 39
        let max_str_length_range = 79
        let max_str_length_network = 43

        let int_of_hex_string s =
            print_endline (" value: " ^ s);
            int_of_string ("0x" ^ s)
        ;;

        let parser_value_part_1 =
            at_most_split 0 8
                (read_16bit_hex <* colon)
                (colon *> read_16bit_hex)
                read_16bit_hex
                (string ":")
        ;;

        let parser_value_part_1_7 =
            at_most_split 0 8
                (read_16bit_hex_7_1 <* colon)
                (colon *> read_16bit_hex_7_1)
                read_16bit_hex_7_1
                (string ":")
        ;;

        let rec at_most_split_2 c m pf pr s f =
            (*
                c = 
                m = max. count
                pf = first part to match
                pr = rest part to match
                s = split part
                f = split part for the first read field
            *)
            match c-m+1 with
            | 0 -> (lift (fun x -> ([x], [])) s)
            | _ ->
                (* Parse (XXXX:)* parts *)
                (lift2 (fun x1 (xs1, xs2) -> (x1 :: xs1, xs2)) 
                        pf 
                        (at_most_split_2 (c+1) m pf pr s f))
                <|>
                (* Parse (:XXXX) parts, special case handling for addresses starting with "::" *)
                (
                    match c with
                    | 0 -> (
                        lift2 (fun _ xs2 -> ([], xs2)) f (limits 1 6 pr)
                        <|>
                        (lift2 (fun _ _ -> ([], [])) f f)
                        )
                    | _ -> (
                        (lift (fun xs -> ([], xs)) (limits 1 (m-c-1) pr))
                        <|>
                        (lift (fun _ -> ([], [])) f)
                    )
                )
        ;;

        let parser_value_part_5 =
            at_most_split_2 0 8
                (read_16bit_hex <* colon)
                (colon *> read_16bit_hex)
                read_16bit_hex
                (string ":")
        ;;

(*
        let parser_value_part_6 =
            (*
                r = remaining positions
                pf = first part to match
                pr = rest part to match
                s = split part
                f = split part for the first read field
            *)
            let s = colon in (* separator *)
            let v = read_16bit_hex_8 in (* value *)
            let vs = v <* s in
            let sv = s *> v in
            let sv4 = s *> IPv4.Parser.parser_ipv4_part in
            let rec parse_start r =
                s *> parse_sv (r-2)
            and parse_sv r =
                sv >>= parse_sv (r-1)
            and parse_vs r =
                vs >>= parse_vs (r-1)
            and at_most_split r =
                if r = 1 then
                begin
                    (lift (fun x -> ([x], [])) v)
                end
                else
                begin
                    (* Parse (XXXX:)* parts *)
                    (lift2 (fun x1 (xs1, xs2) -> (x1 :: xs1, xs2)) 
                            vs 
                            (at_most_split (r-1)))
                    <|>
                    (* Parse (:XXXX) parts, special case handling for addresses starting with "::" *)
                    (
                        if r = 8 then
                        begin
                            lift (fun xs2 -> ([], xs2)) (s *> (limits 1 (r-1) sv))
                            <|>
                            (lift (fun _ -> ([],[])) (s *> s))
                        end
                        else if r < 5 then
                        begin
                            (lift (fun xs -> ([], xs)) (limits 1 (r-1) sv))
                            <|>
                            (lift (fun _ -> ([], [])) s)
                        end
                        else
                        begin
                            (lift (fun xs -> ([], xs)) (limits 1 (r-1) sv))
                            <|>
                            (lift (fun _ -> ([], [])) s)
                        end
                    )
                end
                in
            at_most_split 8
        ;;
*)

        type ps =
        | PS_Start
        | PS_bdc of int
        | PS_adc of int
        | PS_End

        let parser_value_part_2 =
            print_endline "fun called";
            let bdc_parse = read_16bit_hex_5 <* colon in
            let adc_parse = colon *> read_16bit_hex_5 in
            let rec parse state (bdc, adc) =
                match state with
                | PS_Start -> 
                begin
                    (* print_endline "PS_Start"; *)
                    choice [
                        (bdc_parse >>= fun v -> parse (PS_bdc 1) ([v], adc));
                        (colon *> parse (PS_adc 1) (bdc, adc));
                    ]
                end
                | PS_bdc n when n < 7 -> 
                begin
                    (* Printf.printf "PS_bdc %d < 7\n" n; *)
                    choice [
                        (bdc_parse >>=
                            fun v ->
                                (* print_endline "bdc n<7 -> bdc"; *)
                                parse (PS_bdc (n + 1)) (v :: bdc, adc));
                        (adc_parse >>=
                            fun v ->
                                (* print_endline "bdc n<7 -> adc"; *)
                                parse (PS_adc (n + 1)) (bdc, [v]));
                        (colon *> parse PS_End (bdc, adc));
                    ]
                end
                | PS_bdc n when n = 7 -> 
                begin
                    (* Printf.printf "PS_bdc %d = 7\n" n; *)
                    read_16bit_hex_5 >>=
                        fun v ->
                            parse PS_End (v :: bdc, adc)
                end
                | PS_adc n when n < 7 -> 
                begin
                    (* Printf.printf "PS_adc %d < 7\n" n; *)
                    choice [
                        (adc_parse >>=
                            fun v ->
                                parse (PS_adc (n + 1)) (bdc, v :: adc));
                        (colon *> parse PS_End (bdc, adc));
                        (parse PS_End (bdc, adc));
                    ]
                end
                | PS_adc n when n = 7 -> 
                begin
                    (* Printf.printf "PS_adc %d = 7\n" n; *)
                    (* print_endline "adc_parse"; *)
                    choice [
                    (adc_parse >>=
                        fun v ->
                            (* print_endline "Ayayay"; *)
                            parse PS_End (bdc, v :: adc));
                        (parse PS_End (bdc, adc));
                    ]
                end
                | PS_adc _ | PS_bdc _ -> 
                    (* print_endline "badmatch"; *)
                    fail "Adc: wrong char"
                | PS_End _ -> 
                    (* print_endline "PS_End"; *)
                    return (List.rev bdc, List.rev adc)
                in
            parse PS_Start ([], [])
        ;;

        type ps4 =
        | PS4_Start
        | PS4_bdc
        | PS4_adc
        | PS4_End
        let parser_value_part_4 =
            print_endline "fun called";
            let bdc_parse = (read_16bit_hex_6 0 4) <* colon in
            let adc_parse = colon *> (read_16bit_hex_6 0 4) in
            let rec parse (state: ps4) (n: int) ((bdc, adc): int list * int list ) (vt: parse_result) ve =
            begin
                let adc_n = match vt with 
                            | Some_result -> ve :: adc
                            | No_result -> adc
                    in
                let bdc_n = match vt with 
                            | Some_result -> ve :: bdc
                            | No_result -> bdc
                    in
                match state with
                | PS4_Start -> 
                    (bdc_parse >>= parse PS4_bdc 1 (bdc_n, adc_n) Some_result)
                    <|>
                    (colon *> parse PS4_adc (n + 1) (bdc_n, adc_n) No_result 0)
                | PS4_bdc when n < 7 -> 
                    (bdc_parse >>= parse PS4_bdc (n + 1) (bdc, adc) Some_result)
                    <|>
                    (adc_parse >>= parse PS4_adc (n + 1) (bdc, adc) Some_result)
                    <|>
                    (colon *> parse PS4_End n (bdc, adc) No_result 0)
                | PS4_bdc when n = 7 -> 
                    read_16bit_hex_6 0 4 >>= parse PS4_End n (bdc, adc) Some_result
                | PS4_adc when n < 7 -> 
                    (adc_parse >>= parse PS4_adc (n + 1) (bdc, adc) Some_result)
                    <|>
                    (colon *> parse PS4_End n (bdc, adc) No_result 0)
                    <|>
                    (parse PS4_End n (bdc, adc) No_result 0)
                | PS4_adc when n = 7 -> 
                    (adc_parse >>= parse PS4_End n (bdc, adc) Some_result)
                    <|>
                    (parse PS4_End n (bdc, adc) No_result 0)
                | PS4_adc | PS4_bdc -> 
                    (* print_endline "badmatch"; *)
                    fail "Adc: wrong char"
                | PS4_End -> 
                    (* print_endline "PS_End"; *)
                    return (List.rev bdc_n, List.rev adc_n)
            end
                in
            parse PS4_Start 0 ([], []) No_result 0
        ;;
                    
        type parse_state = 
        | Start
        | Before_dc of int
        | After_dc of int * int
        | End 

        let parser_value_part_3 =
            let rec parse state (bdc, adc) =
                match state with
                | Start -> 
                begin
                    (read_16bit_hex <* colon >>=
                        (fun v ->
                            parse (Before_dc 1) (v :: bdc, adc)))
                    <|> (colon >>=
                        (fun _ ->
                            parse (After_dc (0, 0)) (bdc, adc)))
                end 
                | Before_dc bn when bn < 7 ->
                begin
                    ((read_16bit_hex <* colon) >>=
                        (fun v ->
                            parse (Before_dc (bn + 1)) (v :: bdc, adc)))
                    <|> (colon *> read_16bit_hex >>=
                        (fun v ->
                            parse (After_dc (bn, 1)) (bdc, v :: adc)))
                    <|> (colon >>=
                    (fun _ -> parse End (bdc, adc)))
                end
                | Before_dc bn when bn = 8 ->
                begin
                    read_16bit_hex >>=
                        (fun v ->
                            parse End (v :: bdc, []))
                end
                | After_dc (bn, an) when bn + an < 7 ->
                begin
                    ((satisfy is_colon) *> read_16bit_hex) >>=
                        (fun v ->
                            parse (After_dc (bn, (an + 1))) (bdc, v :: adc))
                    <|> (colon >>=
                    (fun _ -> parse End (bdc, adc)))
                end
                | _ -> return (List.rev bdc, List.rev adc)
                in
            parse Start ([],[])
        ;;

        let parser_value_part = parser_value_part_1_7

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

        let find_first_longest_streak element_selector element_list =
            let open Stdlib in
            let detect_list (i, cur_opt, max_opt) value = 
                begin
                let element_selected = element_selector value in
                let new_cur, new_max = 
                    match cur_opt with
                    | None ->
                    begin
                    if element_selected then
                        Some {cur_streak_start = i; cur_streak_len = 1}, max_opt
                    else 
                        None, max_opt
                    end
                    | Some last_cur ->
                    begin
                    if element_selected then
                        Some {last_cur with cur_streak_len = last_cur.cur_streak_len + 1}, max_opt
                    else 
                        match max_opt with
                        | None -> None, Some (max_streak_of_cur_streak last_cur)
                        | Some last_max when last_cur.cur_streak_len > last_max.max_streak_len -> 
                        None, Some (max_streak_of_cur_streak last_cur)
                        | Some _ -> 
                        None, Some {max_streak_start = last_cur.cur_streak_start; 
                                    max_streak_len = last_cur.cur_streak_len}
                    end in
                (succ i), new_cur, new_max
                end 
                in
            match List.fold_left detect_list (0, None, None) element_list with
            | _, Some cur_streak, None ->
                Some (streak_of_cur_streak cur_streak)
            | _, Some cur_streak, Some max_streak when cur_streak.cur_streak_len > max_streak.max_streak_len -> 
                Some (streak_of_cur_streak cur_streak)
            | _, _, Some max_streak -> 
                Some (streak_of_max_streak max_streak)
            | _, _, None -> 
                None
        ;;

        let ints_to_value list =
            (*assert (List.length list = 8);*)
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

        let to_string netaddr =
            (*let shift_list = [112;96;80;64;48;32;16;0] in*)
            (* Shift and 'and' each 16bit part of the value*)
            let b16_values = List.map 
                                (fun sw -> Uint128.(logand (shift_right netaddr sw) mask_16lsb)) 
                                shift_list in
            (* Find the longest list of conscutive zeros to be replaced by :: in the output *)
            (* Convert value to hex, remove '0x' prefix *)
            let uint_to_hex v = 
                let hex_raw = Uint128.to_string_hex v in
                let hex_raw_len = String.length hex_raw in
                String.sub hex_raw 2 Stdlib.(hex_raw_len-2)
                in
            let value_list = List.map uint_to_hex b16_values in
            match find_first_longest_streak (fun e -> Uint128.(compare e zero) = 0) b16_values with
                (* No list of consecutive zeros found, just print every element separated by ':' *)
                | None ->
                String.concat ":" value_list
                (* List of consecutive zeros found, replace it by '::', print every element separated 
                    by ':' elsewhere *)
                | Some streak -> 
                begin
                    let value_array = Array.of_list value_list in
                    let part2_start = streak.streak_start + streak.streak_len in
                    let part2_len = (Array.length value_array) - part2_start in
                    let part1 = Array.to_list (Array.sub value_array 0 streak.streak_start) in
                    let part2 = Array.to_list (Array.sub value_array part2_start part2_len) in
                    String.concat ":" part1 ^ "::" ^ String.concat ":" part2
                end
        ;;

        type either =
        | Value of int
        | Streak of int

        let serialize t netaddr =
            (* [@tailcall] *)
            let rec detect_streak (lso: int option) (cso: int option) (r: either list) (i: int list) : int option * either list =
                match i with
                | [] ->
                begin
                match lso, cso with
                | None   , Some _                            ->  cso, r
                | Some ls, Some cs when Stdlib.(cs > ls) ->  cso, r
                | None   , None                              -> None, r
                | Some _ , None                              ->  lso, r
                | Some _ , Some _                            ->  lso, r
                end
                | ni :: ri -> 
                begin
                let vi = Uint128.(to_int (logand (shift_right netaddr ni) mask_16lsb)) in
                if vi = 0 then
                    match lso, cso with
                    | None  , None    -> detect_streak None (Some 1) r ri
                    | Some _, None    -> detect_streak lso  (Some 1) r ri
                    | None  , Some cs -> detect_streak None (Some (cs + 1)) r ri
                    | Some _, Some cs -> detect_streak lso  (Some (cs + 1)) r ri
                else
                    match lso, cso with
                    | None   , None                              -> detect_streak None None ((Value vi) :: r) ri
                    | Some _ , None                              -> detect_streak  lso None ((Value vi) :: r) ri
                    | None   , Some cs                           -> detect_streak  cso None ((Value vi) :: (Streak cs) :: r) ri
                    | Some ls, Some cs when Stdlib.(cs > ls) -> detect_streak  cso None ((Value vi) :: (Streak cs) :: r) ri
                    | Some ls, Some _                            -> detect_streak  lso None ((Value vi) :: (Streak ls) :: r) ri
                end
                in
            let rec combine (rlso: int option) (rl: either list) first =
                let open Faraday in
                match rl with
                | [] -> ()
                | head :: tails ->
                    match rlso, head, first with
                    | None,    Streak _ , _               -> raise (Parser_error "ERROR: found streak where none should be.")
                    | None,     Value v, true            -> 
                    begin
                        write_string t (string_of_int v);
                        combine None tails false
                    end
                    | None,     Value v, false           -> 
                    begin
                        write_char t ':';
                        write_string t (string_of_int v);
                        combine None tails false
                    end
                    | Some ls, Streak sl, _ when ls = sl -> 
                    begin
                        write_char t ':';
                        combine None tails false
                    end
                    | Some _, Streak sl, _ -> 
                    begin
                        let rec print_streak tsl =
                            if tsl = 0 then
                                write_string t "0"
                            else
                            (
                                write_string t "0:";
                                print_streak (tsl - 1)
                            )
                            in
                        print_streak sl;
                        combine rlso tails first
                    end
                    | Some _,   Value v, true            ->
                    begin
                        write_string t (string_of_int v);
                        combine rlso tails false
                    end
                    | Some _,   Value v, false           ->
                    begin
                        write_char t ':';
                        write_string t (string_of_int v);
                        combine rlso tails false
                    end
                    in
            let min_streak, value_list = detect_streak None None [] shift_list in
            combine min_streak value_list true
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

