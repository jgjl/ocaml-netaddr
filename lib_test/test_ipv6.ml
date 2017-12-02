
let test_str2netaddr_sameinout () =
  let run_twowaytest_sameinout addr_str =
    let netaddrv6_opt = Netaddress.Ipv6.Address.of_string addr_str in
    let stringv6 = match netaddrv6_opt with
      | Some netaddrv6 -> Netaddress.Ipv6.Address.to_string ~out_format:Netaddress.Ipv6.Ipv6Short netaddrv6
      | None -> "" 
    in
    Alcotest.(check string) ("Conversion of " ^ addr_str ^ " from and to netaddr -" ^ stringv6 ^ "-.") addr_str stringv6
  in
  let addr_list = [
    "::";
    "2003::";
    "2003:1001::";
    "::1";
    "::1:2:3:4";
    "::1:2:3:4:5";
    "ffdb::1";
    "2004::4";
    "1:2:3:4:5:6:7:8";
    "1:2:3:4::7:8";
    "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff";
    (*"::ffff:192.168.0.1"; not yet *)
  ] in
  List.iter run_twowaytest_sameinout addr_list

let test_str2netaddr () =
  let run_twowaytest (addr_str_in, addr_str_out) =
    let netaddrv6_opt = Netaddress.Ipv6.Address.of_string addr_str_in in
    let stringv6 = match netaddrv6_opt with
      | Some netaddrv6 -> Netaddress.Ipv6.Address.to_string ~out_format:Netaddress.Ipv6.Ipv6Short netaddrv6
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
    (":", "");
  ] in
  List.iter run_twowaytest addr_list

let suite = [
  "convert ip address from and to netaddr object where input = output", `Quick, test_str2netaddr_sameinout;
  "convert ip address from and to netaddr object where input =/= output", `Quick, test_str2netaddr_sameinout;
]