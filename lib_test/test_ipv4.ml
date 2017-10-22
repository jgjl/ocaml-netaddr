
let test_str2netaddr () =
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

let suite = [
  "convert ip address from and to netaddr object", `Quick, test_str2netaddr;
]
