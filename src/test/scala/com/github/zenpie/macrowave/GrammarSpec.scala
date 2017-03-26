package com.github.zenpie.macrowave

import org.scalatest.{FlatSpec, Matchers}
import shapeless.test.illTyped

class GrammarSpec extends FlatSpec with Matchers {

  behavior of "A grammar"

  it should "not compile, if no start-rule is defined" in {
    illTyped(
      """
      import com.github.zenpie.macrowave._
      @grammar
      class Parser {}
      """,
      "No start-rule is defined!"
    )
  }

  it should "not compile, if multiple start-rules are defined" in {
    illTyped(
      """
      import com.github.zenpie.macrowave._
      @grammar
      class Parser {
        val dummyToken: Token = token("dummy")

        @start val S1: Rule1[String] = dummyToken
        @start val S2: Rule1[String] = dummyToken
      }
      """,
      "Multiple definitions of start-rule: S1, S2!"
    )
  }

}
