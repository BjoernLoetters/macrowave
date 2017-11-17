package com.github.zenpie.macrowave

import org.scalatest.{FlatSpec, Matchers}
import shapeless.test.illTyped

class GrammarSpec extends FlatSpec with Matchers {

  behavior of "A grammar"

  /* operations and rules */

  it should "compile, if it is valid" in {
    import com.github.zenpie.macrowave._
    import scala.language.postfixOps

    @grammar class Parser {
      val DIGIT  = com.github.zenpie.macrowave.regex("[0-9]")

      val NUMBER = token(DIGIT +)

      @start def S: Rule1[String] = (
          ExplicitRuleType
        | InferredRuleType
        | Epsilon
        | Concat ^^ (_ + _)
        | Alternative
        | Optional ^^ (_.toString)
        | PositiveClosue ^^ ((a, b) => a.toString + b.toString)
        | Kleene ^^ (_.toString)
      )

      def ExplicitRuleType: Rule1[String] = NUMBER
      def InferredRuleType = singletonRule(NUMBER)
      def Epsilon: Rule1[String] = epsilon ^^ (() => "empty")
      def Concat: Rule[String :: String :: HNil] = NUMBER ~ NUMBER
      def Alternative: Rule[String :: HNil] = NUMBER | NUMBER
      def Optional: Rule[Option[String :: HNil] :: HNil] = NUMBER ?
      def PositiveClosue: Rule[(String :: HNil) :: List[String :: HNil] :: HNil] = NUMBER +
      def Kleene: Rule[List[String :: HNil] :: HNil] = NUMBER *
    }
  }

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
        @start val S: Rule1[Any] = A

        def A: Rule1[String] = A
      }
      }""",
      "The rule 'A' is useless!"
    )

    illTyped(
      """{
      import com.github.zenpie.macrowave._

      @grammar
      class Parser {
        val dummyToken: Token = token("dummy")

        @start val S: Rule1[Any] = S ~ A ^^ ((s, a) => ())

        def A: Rule1[String] = dummyToken
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

        def A: Rule1[Any] = A ~ B ^^ ((a, b) => ())
        def B: Rule1[String] = dummyToken
      }
      }""",
      "The rule 'A' is useless!"
    )

    illTyped(
      """{
      import com.github.zenpie.macrowave._

      @grammar
      class Parser {
        val dummyToken: Token = token("dummy")

        @start val S: Rule[Any :: List[Any] :: HNil] = A +
        def A: Rule1[Any] = A
      }
      }""",
      "The rule 'A' is useless!"
    )
  }

  it should "compile, if it doesn't contain a useless rule" in {
      import com.github.zenpie.macrowave._

    @grammar
    class Parser1 {
      val dummyToken: Token = token("dummy")

      @start val S: Rule1[Any] = A *
      def A: Rule1[Any] = dummyToken
    }

    @grammar
    class Parser2 {
      val dummyToken: Token = token("dummy")

      @start val S: Rule1[Any] = A ?
      def A: Rule1[Any] = dummyToken
    }
  }

  it should "not throw a stack-overflow-exception, if a production is recursive" in {
    @grammar class Parser {
      import com.github.zenpie.macrowave

      val NUMBER: Token = token(macrowave.regex("[0-9]+"))
      val DIGIT : Token = token(macrowave.regex("[0-9]"))

      @start def S: Rule1[String] = A
      def A: Rule1[String] = B ~ A ^^ (_ + _) | B
      def B: Rule1[String] = NUMBER
    }
  }

}
