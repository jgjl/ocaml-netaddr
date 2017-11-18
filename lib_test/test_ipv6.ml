
let test_str2netaddr () =
  let run_twowaytest addr_str =
    let netaddrv6_opt = Netaddress.Ipv6.of_string addr_str in
    let stringv6 = match netaddrv6_opt with
      | Some netaddrv6 -> Netaddress.Ipv6.to_string netaddrv6
      | None -> "" 
    in
    Alcotest.(check string) ("Conversion of " ^ addr_str ^ " from and to netaddr.") addr_str stringv6
  in
  let addr_list = [
    "::";
    "::1";
    "ffdb::1";
    "2004::4";
    "0:0:0:0:0:0:0:0";
    "1:2:3:4:5:6:7:8";
    "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff";
  ] in
  List.iter run_twowaytest addr_list

let suite = [
  "convert ip address from and to netaddr object", `Quick, test_str2netaddr;
]