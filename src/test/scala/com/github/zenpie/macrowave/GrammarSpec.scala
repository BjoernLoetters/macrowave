package com.github.zenpie.macrowave

import org.scalatest.{FlatSpec, Matchers}
import shapeless.test.illTyped

class GrammarSpec extends FlatSpec with Matchers {

  behavior of "A grammar"

  /* start-rules */

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

  /* whiteSpace token */

  it should "compile, if no whiteSpace token is defined" in {
    import com.github.zenpie.macrowave._
    @grammar
    class Parser {
      val dummyToken: Token = token("dummy")

      @start val S: Rule1[String] = dummyToken
    }
  }

  it should "compile, if one whiteSpace token is defined" in {
    import com.github.zenpie.macrowave._
    @grammar
    class Parser {
      val dummyToken: Token = token("dummy")

      @whiteSpace def ws: Token = token("[ \t\n]".r)

      @start val S: Rule1[String] = dummyToken
    }
  }

  it should "not compile, if multiple whiteSpace tokens are defined" in {
    illTyped(
      """{
      import com.github.zenpie.macrowave._

      @grammar
      class Parser {
        val dummyToken: Token = token("dummy")

        @whiteSpace def ws1 = token("[ \t\n]".r)
        @whiteSpace def ws2 = token("[ \t\n]".r)

        @start val S: Rule1[String] = dummyToken
      }
      }""",
      "Multiple definitions of whiteSpace-token: ws1, ws2!"
    )
  }

}
