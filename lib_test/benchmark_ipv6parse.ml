
exception Error of string

let run_parser parser param =
    match Angstrom.parse_string ~consume:All parser param with
    | Result.Error e -> raise (Error e)
    | Result.Ok r -> r
;;



let read_16bit_hex () =
    print_endline "###### Run read_16bit_hex benchmark";
    let open Benchmark in
    let repetitions = 100000000L in
    let number = "1234" in
    let res_1 = latency1 ~name:"read_16bit_hex_1" repetitions (run_parser Netaddress.Parse_helper.read_16bit_hex_1) number in
    let res_1_1 = latency1 ~name:"read_16bit_hex_1_1" repetitions (run_parser Netaddress.Parse_helper.read_16bit_hex_1_1) number in
    let res_2 = latency1 ~name:"read_16bit_hex_2" repetitions (run_parser Netaddress.Parse_helper.read_16bit_hex_2) number in
    let res_4 = latency1 ~name:"read_16bit_hex_4" repetitions (run_parser Netaddress.Parse_helper.read_16bit_hex_4) number in
    let res_7 = latency1 ~name:"read_16bit_hex_7" repetitions (run_parser Netaddress.Parse_helper.read_16bit_hex_7) number in
    let res_7_1 = latency1 ~name:"read_16bit_hex_7_1" repetitions (run_parser Netaddress.Parse_helper.read_16bit_hex_7_1) number in
    let res_5 = latency1 ~name:"read_16bit_hex_5" repetitions (run_parser Netaddress.Parse_helper.read_16bit_hex_5) number in
    (* let res_6 = latency1 ~name:"read_16bit_hex_6" repetitions (run_parser (Netaddress.Parse_helper.read_16bit_hex_6 0 4)) number in *)
    let res_3 = latency1 ~name:"read_16bit_hex_3" repetitions (run_parser Netaddress.Parse_helper.read_16bit_hex_3) number in
    let res_8 = latency1 ~name:"read_16bit_hex_8" repetitions (run_parser Netaddress.Parse_helper.read_16bit_hex_8) number in
    print_newline();
    tabulate res_1;
    tabulate res_1_1;
    tabulate res_2;
    tabulate res_4;
    tabulate res_7;
    tabulate res_7_1;
    tabulate res_5;
    (* tabulate res_6; *)
    tabulate res_3;
    tabulate res_8;
;;

let parse_ipv6_parts () =
    print_endline "###### Run parse_ipv6_parts benchmark";
    let open Benchmark in
    let repetitions = 10000000L in
    (* let address = "::" in *)
    let address = "1:2345:3789:4011:5213::6415:7fff" in
    (* let address = "1:2345:3789:5213::6415:7fff" in *)
    let res_1 = latency1 ~name:"parser_value_part_1" repetitions (run_parser Netaddress.IPv6.Parser.parser_value_part_1) address in
    let res_5 = latency1 ~name:"parser_value_part_5" repetitions (run_parser Netaddress.IPv6.Parser.parser_value_part_5) address in
    (* let res_6 = latency1 ~name:"parser_value_part_6" repetitions (run_parser Netaddress.IPv6.Parser.parser_value_part_6) address in *)
    let res_2 = latency1 ~name:"parser_value_part_2" repetitions (run_parser Netaddress.IPv6.Parser.parser_value_part_2) address in
    let res_4 = latency1 ~name:"parser_value_part_4" repetitions (run_parser Netaddress.IPv6.Parser.parser_value_part_4) address in
    (* let res_3 = latency1 ~name:"parser_value_part_3" repetitions (run_parser Netaddress.IPv6.Parser.parser_value_part_3) address in *)
    let res_1_7 = latency1 ~name:"parser_value_part_7_1" repetitions (run_parser Netaddress.IPv6.Parser.parser_value_part_1_7) address in
    print_newline();
    tabulate res_1;
    tabulate res_5;
    (* tabulate res_6; *)
    tabulate res_2;
    (* tabulate res_3; *)
    tabulate res_4;
    tabulate res_1_7;
;;

let () =
    read_16bit_hex ();
    parse_ipv6_parts ()
;;