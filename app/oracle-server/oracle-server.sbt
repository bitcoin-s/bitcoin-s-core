import com.typesafe.sbt.packager.docker.DockerChmodType

name := "bitcoin-s-oracle-server"

Universal / packageName := CommonSettings.buildPackageName(
  (Universal / packageName).value)

// Ensure actor system is shut down
// when server is quit
Compile / fork := true

libraryDependencies ++= Deps.oracleServer.value

mainClass := Some("org.bitcoins.oracle.server.OracleServerMain")

packageSummary := "A DLC Oracle"

packageDescription := "A basic DLC oracle that allows you to commit to events and sign them"

dockerExposedPorts ++= Seq(9998)

dockerEntrypoint := Seq("/opt/docker/bin/bitcoin-s-oracle-server")

//so the server can be read and executed by all users
dockerAdditionalPermissions += (DockerChmodType.Custom("a=rx"),
                                "/opt/docker/bin/bitcoin-s-oracle-server")

//this passes in our default configuration for docker
//you can override this by passing in a custom conf file
//when the docker container is started by using bind mount
//https://docs.docker.com/storage/bind-mounts/#start-a-container-with-a-bind-mount
dockerCmd ++= Seq("--conf", "/opt/docker/docker-application.conf")
