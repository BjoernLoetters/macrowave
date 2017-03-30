import sbt._

object RuleActionBoilerplate {

  val targetPackage = Seq("com", "github", "zenpie", "macrowave")
  val maxArity = 22

  def generate(sourceManagedDir: File): Seq[File] = {
    val targetDir  = targetPackage.foldLeft(sourceManagedDir)(_ / _)
    val targetFile = targetDir / "RuleActions.scala"

    val ruleActionClasses = 0 to maxArity map ruleAction
    IO.write(targetFile, s"""
      |package ${targetPackage.mkString(".")}
      |
      |import scala.annotation.compileTimeOnly
      |
      |trait RuleActions {
      |
      |${ruleActionClasses.mkString("\n")}
      |}
     """.stripMargin)

    Seq(targetFile)
  }

  def ruleAction(arity: Int): String = {
    val letters = ('A' to 'Z') take arity
    val abc     = letters mkString ", "
    val `[abc]` = if (arity == 0) "" else s"[$abc]"
    val abcHNil = (letters map (_ + " :: ")).mkString + "HNil"

    s"""  implicit class RuleAction$arity${`[abc]`}(rule: Rule[$abcHNil]) {
       |    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
       |    def ^^[Res](f: ($abc) => Res): Rule[Res :: HNil] = compileTime
       |  }
       |""".stripMargin
  }

}
