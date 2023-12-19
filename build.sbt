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
    "-deprecation",
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

lazy val root = project.in(file(".")).aggregate(tests)

lazy val fsm = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("modules/fsm"))
  .settings(
    name := "dynamical-fsm",
    libraryDependencies ++= Seq(
      "com.julianpeeters" %%% "polynomial"   % "0.0.0+17-d8e702e2+20231217-2013-SNAPSHOT",
      "com.julianpeeters" %%% "destructured" % "0.2.0",
    )
  )

lazy val tests = project.in(file("modules/tests"))
  .settings(
    name := "dynamical-tests",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
  .dependsOn(fsm.jvm)
  .enablePlugins(NoPublishPlugin)

lazy val docs = project.in(file("docs/gitignored"))
  .settings(
    mdocOut := file("."),
    mdocVariables := Map(
      "SCALA" -> crossScalaVersions.value.map(e => e.takeWhile(_ != '.')).mkString(", "),
      "VERSION" -> version.value.takeWhile(_ != '+'),
    )
  )
  .dependsOn(fsm.jvm)
  .enablePlugins(MdocPlugin)
  .enablePlugins(NoPublishPlugin)