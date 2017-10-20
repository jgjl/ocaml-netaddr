
let test_str2netaddr () =
  let netaddrv4 = IPv4.of_string "192.168.0.1" in
  let stringv4 = IPv4.to_string netaddrv4 in
  Alcotest.(check string) "Conversion of 192.168.0.1 from and to netaddr." "192.168.0.1" stringv4

let suite = [
  "convert ip address from and to netaddr object", `Quick, test_str2netaddr;
]
