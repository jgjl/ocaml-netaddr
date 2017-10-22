
let suite = [
  "ipv4"           , Test_ipv4.suite        ;
  (*"ipv6"           , Test_ipv6.suite        ;*)
]


let () =
  Alcotest.run "netaddr" suite
