let run_parser parser param =
    match Angstrom.parse_string ~consume:All parser param with
    | Result.Error _ -> -1
    | Result.Ok r -> r
;;

let test_hex2int () =
  let run_twowaytest (hex_str_in, int_out) =
    let result_int = run_parser Netaddress.Parse_helper.read_16bit_hex_7_1 hex_str_in in
    Alcotest.(check int) ("Conversion of " ^ hex_str_in ^ " to int " ^ (string_of_int int_out) ^ "-.") int_out result_int
  in
  let val_list = [
    ("00", 0);
    ("2003", 8195);
    ("1001", 4097);
    ("0001", 1);
    ("ffff", 65535);
    ("aaaa", 43690);
    (* Failing tests *)
    (":::", -1);
    ("1:::", -1);
    ("1:1:::::", -1);
    ("1:1:1::::", -1);
    (":", -1);
    ("1:2:3:4:5:6:7:8:9", -1);
    ("1:2:3:4:5:6:7:8:9:", -1);
    ("9:", -1);
    (":1", -1);
    ("11111", -1);
    ("00001", -1);
    ("1:2::6:7:", -1);
    ("#", -1);
    ("*", -1);
    ("g", -1);
    ("1:2:3:4:5:6:7:8:", -1);
    ("1::3:4:5::6:7:8", -1);
    ("1:2::4:5:6:7:8", -1);
    ("1:2:3::5:6:7:8", -1);
    ("1:2:3:4::6:7:8", -1);
    ("1:2:3:4:5::7:8", -1);
    ("1:2:3:4:5:6:::8", -1);
    ("1:2:3:4:5::6::", -1);
    ("1::4:5:6::7:8", -1);
    ("1:2:::5:6:7:8", -1);
    ("1:2:3:::6:7:8", -1);
    ("1:2:3:4:::7:8", -1);
    ("1:2:3:4:5:::8", -1);
  ] in
  List.iter run_twowaytest val_list

let test_str2netaddr_sameinout () =
  let run_twowaytest_sameinout addr_str =
    let netaddrv6_opt = Netaddress.IPv6.Address.of_string addr_str in
    let stringv6 = match netaddrv6_opt with
      | Some netaddrv6 -> Netaddress.IPv6.Address.to_string netaddrv6
      | None -> ""
    in
    Alcotest.(check string) ("Conversion of " ^ addr_str ^ " from and to netaddr -" ^ stringv6 ^ "-.") addr_str stringv6
  in
  let addr_list = [
    "::";
    "2003::";
    "2003:1001::";
    "::12";
    "::1";
    "::1:2:3:4";
    "::1:2:3:4:5";
    "ffdb::1";
    "2004::4";
    "1:2:3:4:5:6:7:8";
    "1::3:4:5:6:7:8";
    "1:2::4:5:6:7:8";
    "1:2:3::5:6:7:8";
    "1:2:3:4::6:7:8";
    "1:2:3:4:5::7:8";
    "1:2:3:4:5:6::8";
    "1:2:3:4:5:6::";
    "1::4:5:6:7:8";
    "1:2::5:6:7:8";
    "1:2:3::6:7:8";
    "1:2:3:4::7:8";
    "1:2:3:4:5::8";
    "1:2:3:4:5:6::";
    "1::5:6:7:8";
    "1:2::6:7:8";
    "1:2:3::7:8";
    "1:2:3:4::";
    "1::6:7:8";
    "1:2::7:8";
    "1:2:3::8";
    "1:2:3:4::";
    "1::7:8";
    "1:2::8";
    "1:2:3::";
    "1::8";
    "1:2::";
    "1::";
    "1:2345:6789:1011:1213::1415:ffff";
    "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff";
    (* "0:0:0:0:0:0:192.168.0.1"; 
    "::192.168.0.1"; 
    "::ffff:192.168.0.1";  *)
  ] in
  List.iter run_twowaytest_sameinout addr_list

let test_str2netaddr () =
  let run_twowaytest (addr_str_in, addr_str_out) =
    let netaddrv6_opt = Netaddress.IPv6.Address.of_string addr_str_in in
    let stringv6 = match netaddrv6_opt with
      | Some netaddrv6 -> Netaddress.IPv6.Address.to_string netaddrv6
      | None -> "" 
    in
    Alcotest.(check string) ("Conversion of " ^ addr_str_in ^ " from and to netaddr " ^ addr_str_out ^ "-.") addr_str_out stringv6
  in
  let addr_list = [
    ("::0:0", "::");
    ("2003::0:00", "2003::");
    ("2003:1001:0:0:0:0:0:0", "2003::");
    ("0:0::1", "::1");
    ("ffdb::0:01", "ffdb::");
    ("2004:0:0:0000:4", "2004::4");
    ("0:0:0:0:0:0:0:0", "::");
    ("0000:0000:0000:0000:0000:0000:0000:0000", "::");
    ("0001:002:03:004::7:8", "1:2:3:4::7:8");
    ("::ffff:192.168.0.1", "::ffff::c0a8:1");
    ("0000:00:0::ffff:192.168.0.1", "::ffff::c0a8:1");
    (* Failing tests *)
    (":::", "");
    (":", "");
    ("1:2:3:4:5:6:7:8:9", "");
    ("9:", "");
    (":1", "");
    ("1", "");
    ("1:2::6:7:", "");
  ] in
  List.iter run_twowaytest addr_list

let test_str2network_pos () =
  let run_twowaytest addr_str =
    let networkv6_opt = Netaddress.IPv6.Network.of_string addr_str in
    let stringv6 = match networkv6_opt with
      | Some netaddrv6 -> Netaddress.IPv6.Network.to_string netaddrv6
      | None -> ""
    in
    Alcotest.(check string) ("Conversion of " ^ addr_str ^ " from and to netaddr.") addr_str stringv6
  in
  let network_list = [
    "2001:34:34::/48";
    "1000::/12";
    (* "::ffff:172.16.0.0/108"; Not implemented yet*)
    "2001:34:54:65::/64";
  ] in
  let rec add_full_prefix_range test_list prefix_len =
    if prefix_len < 0 then
      test_list
    else
      add_full_prefix_range (("::/" ^ string_of_int prefix_len) :: test_list) (prefix_len - 1)
    in
  let addr_list_complete = add_full_prefix_range network_list Netaddress.IPv6.Address.bit_size in
  List.iter run_twowaytest network_list
;;


let test_str2network_neg () =
  let run_twowaytest network_str =
    let networkv6_opt = Netaddress.IPv6.Network.of_string network_str in
    let stringv6 = match networkv6_opt with
      | Some networkv6 -> Netaddress.IPv6.Network.to_string networkv6
      | None -> ""
    in
    print_string stringv6;
    Alcotest.(check string) ("Conversion of " ^ network_str ^ " from and to netaddr.") "" stringv6
  in
  let addr_list = [
    ":1";
    "2001/4";
    "::/129";
    "11::11/129";
    "1000::0/-1";
    "::256/1";
    "2001:34:54:65:33:ffff::/10";
    "10.0.0.0.9";
    "10.0.0.0-8";
    "172.16.0.0:12";
    "0.0.-0.0/0";
  ] in
  List.iter run_twowaytest addr_list
;;

let suite = [
    "convert hex values to integer values", `Quick, test_hex2int;
    "convert ip address, input =/= output", `Quick, test_str2netaddr_sameinout;
    "convert ip network, positive tests", `Quick, test_str2network_pos;
    "convert ip network, negative tests", `Quick, test_str2network_neg;
]
