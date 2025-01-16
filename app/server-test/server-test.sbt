name := "bitcoin-s-server-test"

publish / skip := true

Test / test := (Test / test dependsOn {
  Projects.bitcoindRpc / TaskKeys.downloadBitcoind
}).value
