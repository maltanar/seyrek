val chiselVersion = System.getProperty("chiselVersion", "latest.release")

val scalaVer = System.getProperty("scalaVer", "2.11.6")

lazy val seyrekSettings = Seq (
  version := "0.1",
  name := "seyrek",

  scalaVersion := scalaVer,

  libraryDependencies ++= ( if (chiselVersion != "None" ) ("edu.berkeley.cs" %% "chisel" % chiselVersion) :: Nil; else Nil),

  libraryDependencies += "com.novocode" % "junit-interface" % "0.10" % "test",
  libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVer
)

lazy val fpgatidbits = project in file("fpga-tidbits")

lazy val seyrek = (project in file(".")).settings(seyrekSettings: _*).dependsOn(fpgatidbits)
mainClass in Compile := Some("Seyrek.SeyrekMainObj")
