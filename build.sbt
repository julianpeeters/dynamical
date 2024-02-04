val CatsV = "2.10.0"
val DestructuredV = "0.2.0"
val Fs2V = "3.9.3"
val MUnitV = "0.7.29"
val PolynomialV = "0.4.0+9-4a9eba9b+20240203-1620-SNAPSHOT"

inThisBuild(List(
  crossScalaVersions := Seq(scalaVersion.value),
  description := "Mode-dependent dynamical systems",
  organization := "com.julianpeeters",
  homepage := Some(url("https://github.com/julianpeeters/dynamical")),
  licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers := List(
    Developer(
      "julianpeeters",
      "Julian Peeters",
      "julianpeeters@gmail.com",
      url("http://github.com/julianpeeters")
    )
  ),
  scalacOptions ++= Seq(
    "-feature",
    "-source:future",
    "-Werror",
    "-Wunused:all",
    "-Wvalue-discard",
    "-Ykind-projector:underscores"
  ),
  scalaVersion := "3.3.1",
  versionScheme := Some("semver-spec"),
))

lazy val root = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("."))
  .enablePlugins(NoPublishPlugin)
  .settings(name := "dynamical")
  .jsSettings(test := {})
  .nativeSettings(test := {})
  .aggregate(fs2, fsm)

lazy val fs2 = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("modules/fs2"))
  .settings(
    name := "dynamical-fs2",
    libraryDependencies ++= Seq(
      "co.fs2"         %%% "fs2-core" % Fs2V,
      "org.scalameta"   %% "munit"    % MUnitV % Test,
    )
  )
  .dependsOn(fsm)
  .jsSettings(test := {})
  .nativeSettings(test := {})

lazy val fsm = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("modules/fsm"))
  .settings(
    name := "dynamical-fsm",
    libraryDependencies ++= Seq(
      "com.julianpeeters" %%% "destructured" % DestructuredV,
      "com.julianpeeters" %%% "polynomial"   % PolynomialV,
      "org.typelevel"     %%% "cats-core"    % CatsV,
      "org.scalameta"      %% "munit"        % MUnitV         % Test
    )
  )
  .jsSettings(test := {})
  .nativeSettings(test := {})

lazy val docs = project.in(file("docs/gitignored"))
  .settings(
    mdocOut := file("."),
    mdocVariables := Map(
      "SCALA"        -> crossScalaVersions.value.map(e => e.takeWhile(_ != '.')).mkString(", "),
      "VERSION"      -> version.value.takeWhile(_ != '+'),
      "DESTRUCTURED" -> DestructuredV.reverse.dropWhile(_ != '.').drop(1).reverse,
      "FS2"          -> Fs2V.reverse.dropWhile(_ != '.').drop(1).reverse,
      "POLYNOMIAL"   -> PolynomialV.reverse.dropWhile(_ != '.').drop(1).reverse,
    )
  )
  .dependsOn(fsm.jvm, fs2.jvm)
  .enablePlugins(MdocPlugin)
  .enablePlugins(NoPublishPlugin)