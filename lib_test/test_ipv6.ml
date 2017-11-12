
let test_str2netaddr () =
  let stringv6 = Netaddress.IPv6.to_string Netaddress.IPv6.one in
  Alcotest.(check string) "Conversion of ::1 from and to netaddr." "::1" stringv6

let suite = [
  "convert ip address from and to netaddr object", `Quick, test_str2netaddr;
]