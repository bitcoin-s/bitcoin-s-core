Test / test := (Test / test dependsOn {
  Projects.bitcoindRpc / TaskKeys.downloadBitcoind
}).value

Test / test := (Test / test dependsOn {
  Projects.lndRpc / TaskKeys.downloadLnd
}).value
