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

  it should "not compile, if a start-rules is defined twice" in {
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
      "The start-rule is already defined \\(S1\\)!"
    )
  }

  it should "not compile, if a start-rules is defined thrice" in {
    illTyped(
      """
      import com.github.zenpie.macrowave._
      @grammar
      class Parser {
        val dummyToken: Token = token("dummy")

        @start val S1: Rule1[String] = dummyToken
        @start val S2: Rule1[String] = dummyToken
        @start val S3: Rule1[String] = dummyToken
      }
      """,
      "The start-rule is already defined \\(S1\\)!"
    )
  }

  /* whiteSpace token */

  it should "compile, if no white space token is defined" in {
    import com.github.zenpie.macrowave._
    @grammar
    class Parser {
      val dummyToken: Token = token("dummy")

      @start val S: Rule1[String] = dummyToken
    }
  }

  it should "compile, if one white space token is defined" in {
    import com.github.zenpie.macrowave._
    @grammar
    class Parser {
      val dummyToken: Token = token("dummy")

      @whiteSpace def ws: Token = token("[ \t\n]".r)

      @start val S: Rule1[String] = dummyToken
    }
  }

  it should "not compile, if a white space token is defined twice" in {
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
      "The white space token is already defined \\(ws1\\)!"
    )
  }

  it should "not compile, if a white space token is defined thrice" in {
    illTyped(
      """{
      import com.github.zenpie.macrowave._

      @grammar
      class Parser {
        val dummyToken: Token = token("dummy")

        @whiteSpace def ws1 = token("[ \t\n]".r)
        @whiteSpace def ws2 = token("[ \t\n]".r)
        @whiteSpace def ws3 = token("[ \t\n]".r)

        @start val S: Rule1[String] = dummyToken
      }
      }""",
      "The white space token is already defined \\(ws1\\)!"
    )
  }

  /* unreachable rules */

  it should "not compile, if it contains a unreachable rule" in {
    illTyped(
      """{
      import com.github.zenpie.macrowave._

      @grammar
      class Parser {
        val dummyToken: Token = token("dummy")

        @start val S: Rule1[String] = A

        def A: Rule1[String] = dummyToken
        def B: Rule1[String] = dummyToken
      }
      }""",
      "The rule 'B' isn't reachable!"
    )
  }

  /* useless rules */

  it should "not compile, if it contains a useless rule" in {
    illTyped(
      """{
      import com.github.zenpie.macrowave._

      @grammar
      class Parser {
        val dummyToken: Token = token("dummy")

        @start val S: Rule1[Any] = S
      }
      }""",
      "The start-rule 'S' is useless!"
    )

    illTyped(
      """{
      import com.github.zenpie.macrowave._

      @grammar
      class Parser {
        val dummyToken: Token = token("dummy")

        @start val S: Rule1[Any] = A

        def A: Rule1[String] = A
      }
      }""",
      "The rule 'A' is useless!"
    )
  }

}
