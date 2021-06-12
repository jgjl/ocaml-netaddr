
let suite = [
  "ipv4"           , Test_ipv4.suite        ;
  "ipv6"           , Test_ipv6.suite        ;
]


let () =
    let argv = Array.of_list ["--verbose"] in
    Alcotest.run ~argv "netaddr" suite
