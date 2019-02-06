
exception Error of string

let run_parser parser param =
    match Angstrom.parse_string parser param with
    | Result.Error e -> raise (Error e)
    | Result.Ok r -> r
;;



let read_16bit_hex () =
    print_endline "###### Run read_16bit_hex benchmark";
    let open Benchmark in
    let repetitions = 100000000L in
    let res_1 = latency1 ~name:"read_16bit_hex_1" repetitions (run_parser Netaddress_parser.read_16bit_hex_1) "1234" in
    let res_2 = latency1 ~name:"read_16bit_hex_2" repetitions (run_parser Netaddress_parser.read_16bit_hex_2) "1234" in
    let res_4 = latency1 ~name:"read_16bit_hex_4" repetitions (run_parser Netaddress_parser.read_16bit_hex_4) "1234" in
    let res_3 = latency1 ~name:"read_16bit_hex_3" repetitions (run_parser Netaddress_parser.read_16bit_hex_3) "1234" in
    print_newline();
    tabulate res_1;
    tabulate res_2;
    tabulate res_4;
    tabulate res_3;
;;

let parse_ipv6_parts () =
    print_endline "###### Run parse_ipv6_parts benchmark";
    let open Benchmark in
    let repetitions = 100000L in
    let address = "1:2345:6789:1011:1213::1415:ffff" in
    let res_1 = latency1 ~name:"parser_value_part_1" repetitions (run_parser Netaddress_parser.IPv6.parser_value_part_1) address in
    let res_2 = latency1 ~name:"parser_value_part_2" repetitions (run_parser Netaddress_parser.IPv6.parser_value_part_2) address in
    let res_3 = latency1 ~name:"parser_value_part_3" repetitions (run_parser Netaddress_parser.IPv6.parser_value_part_3) address in
    print_newline();
    tabulate res_1;
    tabulate res_2;
    tabulate res_3;
;;

let () =
    (* read_16bit_hex (); *)
    parse_ipv6_parts ()
;;