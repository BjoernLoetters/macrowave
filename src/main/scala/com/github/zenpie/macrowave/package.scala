package com.github.zenpie

import scala.annotation.compileTimeOnly
import scala.language.implicitConversions

package object macrowave {

  type HList = shapeless.HList
  type ::[H, T <: HList] = shapeless.::[H, T]

  type HNil = shapeless.HNil
  val HNil = shapeless.HNil


  def compileTime: Nothing = throw new UnsupportedOperationException("")


  @compileTimeOnly("Calls to function 'token' have to be inside a macro invocation!")
  implicit def token(regex: RegExp): Rule[String :: HNil] = compileTime


  @compileTimeOnly("Calls to function 'literal' have to be inside a macro invocation!")
  implicit def literal(string: String): RegExp = compileTime

  @compileTimeOnly("Calls to function 'regex' have to be inside a macro invocation!")
  def regex(string: String): RegExp = compileTime

  /*
     TODO: Let SBT do the (here) hardcoded code-generation

     def mapfn(cnt: Int): String = {
       val letters = ('A' to 'Z') take cnt
       val abc     = letters mkString ", "
       val `[abc]` = if (cnt == 0) "" else s"[$abc]"
       val abcHNil = (letters map (_ + " :: ")).mkString + "HNil"

       s"""implicit class RuleAction$cnt${`[abc]`}(rule: Rule[$abcHNil]) {
        |  @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
        |  def ^^[Res](f: ($abc) => Res): Rule[Res :: HNil] = compileTime
        |}""".stripMargin
     }

     (0 to 22) foreach { i =>
        println(mapfn(i))
        println()
     }

   */

  implicit class RuleAction0(rule: Rule[HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: () => Res): Rule[Res :: HNil] = compileTime
  }

  implicit class RuleAction1[A](rule: Rule[A :: HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: (A) => Res): Rule[Res :: HNil] = compileTime
  }

  implicit class RuleAction2[A, B](rule: Rule[A :: B :: HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: (A, B) => Res): Rule[Res :: HNil] = compileTime
  }

  implicit class RuleAction3[A, B, C](rule: Rule[A :: B :: C :: HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: (A, B, C) => Res): Rule[Res :: HNil] = compileTime
  }

  implicit class RuleAction4[A, B, C, D](rule: Rule[A :: B :: C :: D :: HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: (A, B, C, D) => Res): Rule[Res :: HNil] = compileTime
  }

  implicit class RuleAction5[A, B, C, D, E](rule: Rule[A :: B :: C :: D :: E :: HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: (A, B, C, D, E) => Res): Rule[Res :: HNil] = compileTime
  }

  implicit class RuleAction6[A, B, C, D, E, F](rule: Rule[A :: B :: C :: D :: E :: F :: HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: (A, B, C, D, E, F) => Res): Rule[Res :: HNil] = compileTime
  }

  implicit class RuleAction7[A, B, C, D, E, F, G](rule: Rule[A :: B :: C :: D :: E :: F :: G :: HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: (A, B, C, D, E, F, G) => Res): Rule[Res :: HNil] = compileTime
  }

  implicit class RuleAction8[A, B, C, D, E, F, G, H](rule: Rule[A :: B :: C :: D :: E :: F :: G :: H :: HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: (A, B, C, D, E, F, G, H) => Res): Rule[Res :: HNil] = compileTime
  }

  implicit class RuleAction9[A, B, C, D, E, F, G, H, I](rule: Rule[A :: B :: C :: D :: E :: F :: G :: H :: I :: HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: (A, B, C, D, E, F, G, H, I) => Res): Rule[Res :: HNil] = compileTime
  }

  implicit class RuleAction10[A, B, C, D, E, F, G, H, I, J](rule: Rule[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: (A, B, C, D, E, F, G, H, I, J) => Res): Rule[Res :: HNil] = compileTime
  }

  implicit class RuleAction11[A, B, C, D, E, F, G, H, I, J, K](rule: Rule[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: (A, B, C, D, E, F, G, H, I, J, K) => Res): Rule[Res :: HNil] = compileTime
  }

  implicit class RuleAction12[A, B, C, D, E, F, G, H, I, J, K, L](rule: Rule[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: (A, B, C, D, E, F, G, H, I, J, K, L) => Res): Rule[Res :: HNil] = compileTime
  }

  implicit class RuleAction13[A, B, C, D, E, F, G, H, I, J, K, L, M](rule: Rule[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: M :: HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Res): Rule[Res :: HNil] = compileTime
  }

  implicit class RuleAction14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](rule: Rule[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: M :: N :: HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Res): Rule[Res :: HNil] = compileTime
  }

  implicit class RuleAction15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](rule: Rule[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: M :: N :: O :: HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Res): Rule[Res :: HNil] = compileTime
  }

  implicit class RuleAction16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](rule: Rule[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: M :: N :: O :: P :: HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Res): Rule[Res :: HNil] = compileTime
  }

  implicit class RuleAction17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](rule: Rule[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: M :: N :: O :: P :: Q :: HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Res): Rule[Res :: HNil] = compileTime
  }

  implicit class RuleAction18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](rule: Rule[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: M :: N :: O :: P :: Q :: R :: HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Res): Rule[Res :: HNil] = compileTime
  }

  implicit class RuleAction19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](rule: Rule[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: M :: N :: O :: P :: Q :: R :: S :: HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Res): Rule[Res :: HNil] = compileTime
  }

  implicit class RuleAction20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](rule: Rule[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: M :: N :: O :: P :: Q :: R :: S :: T :: HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Res): Rule[Res :: HNil] = compileTime
  }

  implicit class RuleAction21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](rule: Rule[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: M :: N :: O :: P :: Q :: R :: S :: T :: U :: HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Res): Rule[Res :: HNil] = compileTime
  }

  implicit class RuleAction22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](rule: Rule[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: M :: N :: O :: P :: Q :: R :: S :: T :: U :: V :: HNil]) {
    @compileTimeOnly("Calls to function '^^' have to be inside a macro invocation!")
    def ^^[Res](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Res): Rule[Res :: HNil] = compileTime
  }

}
