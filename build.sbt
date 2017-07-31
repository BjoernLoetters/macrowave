
name         := "macrowave"

organization := "com.github.zen-pie"

version      := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.8"

resolvers += Resolver.sonatypeRepo("releases")

scalacOptions ++= Seq("-deprecation", "-feature")

addCompilerPlugin(
  "org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full
)

libraryDependencies ++= Seq(
  "com.chuusai"            %% "shapeless"                % "2.3.2",
  "org.scalatest"          %% "scalatest"                % "3.0.1" % "test",
  "org.scala-lang"         % "scala-reflect"             % scalaVersion.value,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5"
)

lazy val generateRuleActions = taskKey[Seq[File]]("Generates the RuleAction boilerplate mixin")

generateRuleActions := RuleActionBoilerplate.generate((sourceManaged in Compile).value)

(sourceGenerators in Compile) += generateRuleActions.taskValue
