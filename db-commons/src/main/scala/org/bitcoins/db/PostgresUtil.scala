package org.bitcoins.db

import org.bitcoins.keymanager.config.KeyManagerAppConfig

object PostgresUtil {

  val schemaNameMaxLen: Int = 63

  def getSchemaName(moduleName: String, walletName: String): String = {
    if (walletName == KeyManagerAppConfig.DEFAULT_WALLET_NAME) {
      moduleName
    } else {
      val schemaName = s"${moduleName}_$walletName"
      require(schemaName.length <= schemaNameMaxLen,
              s"Schema name length must be up to 63 characters: `$schemaName`")
      schemaName
    }
  }
}
