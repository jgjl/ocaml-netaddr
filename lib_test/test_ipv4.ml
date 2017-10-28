
let test_str2netaddr_pos () =
  let run_twowaytest addr_str =
    let netaddrv4_opt = Netaddr.IPv4.of_string addr_str in
    let stringv4 = match netaddrv4_opt with
      | Some netaddrv4 -> Netaddr.IPv4.to_string netaddrv4
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

let test_str2netaddr_neg () =
  let run_twowaytest addr_str =
    let netaddrv4_opt = Netaddr.IPv4.of_string addr_str in
    let stringv4 = match netaddrv4_opt with
      | Some netaddrv4 -> Netaddr.IPv4.to_string netaddrv4
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

let suite = [
  "convert ip address from and to netaddr object, positive tests", `Quick, test_str2netaddr_pos;
  "convert ip address from and to netaddr object, negative tests", `Quick, test_str2netaddr_neg;
]
