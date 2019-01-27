
let test_str2netaddr_pos () =
  let run_twowaytest addr_str =
    let netaddrv4_opt = Netaddress.IPv4.Address.of_string addr_str in
    let stringv4 = match netaddrv4_opt with
      | Some netaddrv4 -> Netaddress.IPv4.Address.to_string netaddrv4
      | None -> ""
    in
    Alcotest.(check string) ("Conversion of " ^ addr_str ^ " from and to netaddr.") addr_str stringv4
  in
  let addr_list = [
    "192.168.0.0";
    "192.168.0.1";
    "192.168.0.255";
    "10.10.0.0";
    "10.10.0.1";
    "10.10.0.255";
    "172.16.0.0";
    "172.16.0.1";
    "172.16.0.255";
    "0.0.0.0";
    "255.255.255.255";
  ] in
  List.iter run_twowaytest addr_list
;;


let test_str2netaddr_neg () =
  let run_twowaytest addr_str =
    let netaddrv4_opt = Netaddress.IPv4.Address.of_string addr_str in
    let stringv4 = match netaddrv4_opt with
      | Some netaddrv4 -> Netaddress.IPv4.Address.to_string netaddrv4
      | None -> ""
    in
    print_string stringv4;
    Alcotest.(check string) ("Conversion of " ^ addr_str ^ " from and to netaddr.") "" stringv4
  in
  let addr_list = [
    "192.168.0.";
    "192.168.0.-1";
    "192.168.0.256";
    "10.10.0.0.1";
    "aa.0b10.vv.hfr.1";
    "10.10.0.256";
    "4093.16.0.1";
    "-1.16.0.255";
    "0.0.0.";
    ".0.0.";
    "........";
    "255.255.255.2554";
    (*
    Not sure what to do with these tests
    "10.1.-00.1";
    "172.16.0.-0";
    *)
  ] in
  List.iter run_twowaytest addr_list
;;


let test_str2network_pos () =
  let run_twowaytest addr_str =
    let networkv4_opt = Netaddress.IPv4.Network.of_string addr_str in
    let stringv4 = match networkv4_opt with
      | Some netaddrv4 -> Netaddress.IPv4.Network.to_string netaddrv4
      | None -> ""
    in
    Alcotest.(check string) ("Conversion of " ^ addr_str ^ " from and to netaddr.") addr_str stringv4
  in
  let addr_list = [
    "192.168.0.0/16";
    "0.0.0.0/32";
    "0.0.0.0/31";
    "0.0.0.0/30";
    "0.0.0.0/29";
    "0.0.0.0/28";
    "0.0.0.0/27";
    "0.0.0.0/26";
    "0.0.0.0/25";
    "0.0.0.0/24";
    "0.0.0.0/23";
    "0.0.0.0/22";
    "0.0.0.0/21";
    "0.0.0.0/20";
    "0.0.0.0/19";
    "0.0.0.0/18";
    "0.0.0.0/17";
    "0.0.0.0/16";
    "0.0.0.0/15";
    "0.0.0.0/14";
    "0.0.0.0/13";
    "0.0.0.0/12";
    "0.0.0.0/11";
    "0.0.0.0/10";
    "0.0.0.0/9";
    "0.0.0.0/8";
    "0.0.0.0/7";
    "0.0.0.0/6";
    "0.0.0.0/5";
    "0.0.0.0/4";
    "0.0.0.0/3";
    "0.0.0.0/2";
    "0.0.0.0/1";
    "0.0.0.0/0";
    "172.16.0.0/12";
  ] in
  List.iter run_twowaytest addr_list
;;


let test_str2network_neg () =
  let run_twowaytest network_str =
    let networkv4_opt = Netaddress.IPv4.Network.of_string network_str in
    let stringv4 = match networkv4_opt with
      | Some networkv4 -> Netaddress.IPv4.Network.to_string networkv4
      | None -> ""
    in
    print_string stringv4;
    Alcotest.(check string) ("Conversion of " ^ network_str ^ " from and to netaddr.") "" stringv4
  in
  let addr_list = [
    "10.0.0.0/4";
    "192.168.0.0/33";
    "10.0.0.0/-1";
    "0.0.0.256/1";
    "10.0.0.0./10";
    "10.0.0.0.9";
    "10.0.0.0-8";
    "172.16.0.0:12";
    "0.0.-0.0/0";
  ] in
  List.iter run_twowaytest addr_list
;;


let suite = [
  "convert ip address from and to netaddr object, positive tests", `Quick, test_str2netaddr_pos;
  "convert ip address from and to netaddr object, negative tests", `Quick, test_str2netaddr_neg;
  "convert ip network from and to netaddr object, positive tests", `Quick, test_str2network_pos;
  "convert ip network from and to netaddr object, positive tests", `Quick, test_str2network_neg;
];;
