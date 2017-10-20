
let test_str2netaddr () =
  let netaddrv6 = IPv6.of_string "::1" in
  let stringv6 = IPv6.to_string netaddrv6 in
  Alcotest.(check string) "Conversion of ::1 from and to netaddr." "::1" stringv6

let suite = [
  "convert ip address from and to netaddr object", `Quick, test_str2netaddr;
]