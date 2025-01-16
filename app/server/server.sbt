import com.typesafe.sbt.packager.docker.DockerChmodType
name := s"bitcoin-s-server"

Universal / packageName := {
  val old = (Universal / packageName).value
  CommonSettings.buildPackageName(old)
}

Compile / fork := true

libraryDependencies ++= Deps.server.value

mainClass := Some("org.bitcoins.server.BitcoinSServerMain")

packageSummary := "A Bitcoin neutrino node and wallet"

packageDescription := "Runs a Bitcoin neutrino node and wallet, has functionality " +
  "for many different modes and configuration options, see more at https://bitcoin-s.org/docs/applications/server"

dockerExposedPorts ++= Seq(9999, 19999)

dockerEntrypoint := Seq("/opt/docker/bin/bitcoin-s-server")

//so the server can be read and executed by all users
dockerAdditionalPermissions += (DockerChmodType.Custom("a=rx"),
                                "/opt/docker/bin/bitcoin-s-server")

//this passes in our default configuration for docker
//you can override this by passing in a custom configuration
//when the docker container is started by using bind mount
//https://docs.docker.com/storage/bind-mounts/#start-a-container-with-a-bind-mount
dockerCmd ++= Seq("--conf", "/opt/docker/docker-application.conf")
