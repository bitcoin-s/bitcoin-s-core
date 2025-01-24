Test / test := (Test / test dependsOn {
  Projects.clightningRpc / TaskKeys.downloadCLightning
}).value

Test / test := (Test / test dependsOn {
  Projects.bitcoindRpc / TaskKeys.downloadBitcoind
}).value
