name := s"bitcoin-s-cli"

Universal / packageName := CommonSettings.buildPackageName(
  (Universal / packageName).value)

libraryDependencies ++= Deps.cli.value

nativeImageInstalled := true

nativeImageOptions ++= Seq(
  "-H:+ReportExceptionStackTraces",
  "--initialize-at-build-time",
  "--no-fallback",
  "--enable-http",
  "--enable-https"
)
