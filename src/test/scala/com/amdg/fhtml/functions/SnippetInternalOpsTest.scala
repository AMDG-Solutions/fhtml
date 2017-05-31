package com.amdg.fhtml.functions

import com.amdg.fhtml.core.LibSpec
import com.amdg.fhtml.core.generators.StringGenerators.whitespacesStrings
import com.amdg.fhtml.core.matchers.EitherMatchers._
import com.amdg.fhtml.types.Snippet
import org.scalatest.prop.PropertyChecks

class SnippetInternalOpsTest extends LibSpec with PropertyChecks {

  import SnippetInternalOps._

  "trimLeadingWhitespaces" should {

    "do nothing if there are no leading whitespaces" in {
      Snippet("abc").trimLeadingWhitespaces shouldBe Right(Snippet("abc"))
    }

    "move the startIdx of the snippet to point at the first non-whitespace character" in {
      forAll(whitespacesStrings) { whitespaces =>
        Snippet(s"${whitespaces}abc").trimLeadingWhitespaces should (be('right) and have(startIdx(whitespaces.length)))
      }
    }
  }

  "indexOfFirst(expression, offset)" should {

    "find index where given expression occurs for the first time - case with snippet startIdx = 0" in {
      Snippet("abc").indexOfFirst("bc") shouldBe Right(1)
    }

    "find index where given expression occurs for the first time - case with snippet startIdx > 0" in {
      Snippet("abc").moveStartIdx(1).indexOfFirst("bc") shouldBe Right(1)
    }

    "find index where given expression occurs for the first time including the offset" in {
      Snippet("abcb").indexOfFirst("b", offset = 2) shouldBe Right(3)
    }

    "return Left with ExtractionError if the given expression does not occur in the snippet" in {
      Snippet("abc").indexOfFirst("d") shouldBe Left(ExtractionError("No 'd' found"))
    }

    "return Left with ExtractionError if the given expression occurs but it's before the current snippet's startIdx" in {
      Snippet("abc").moveStartIdx(2).indexOfFirst("b") shouldBe Left(ExtractionError("No 'b' found"))
    }

    "return Left with ExtractionError if the given expression occurs but before the given offset" in {
      Snippet("abc").indexOfFirst("b", offset = 2) shouldBe Left(ExtractionError("No 'b' found"))
    }
  }

  "indexOfFirst(expression, before, offset)" should {

    "find index where given expression occurs for the first time - case with snippet startIdx = 0" in {
      Snippet("abc").indexOfFirst("b", before = 2, offset = 0) shouldBe Right(1)
    }

    "find index where given expression occurs for the first time - case with snippet startIdx > 0" in {
      Snippet("abc").moveStartIdx(1).indexOfFirst("b", before = 2, offset = 0) shouldBe Right(1)
    }

    "find index where given expression occurs for the first time including the offset" in {
      Snippet("abcb").indexOfFirst("b", before = 4, offset = 2) shouldBe Right(3)
    }

    "return Left with ExtractionError if the given expression does occur but not before the given index" in {
      Snippet("abc").indexOfFirst("b", before = 1, offset = 0) shouldBe Left(ExtractionError("No 'b' found"))
    }

    "return Left with ExtractionError if the given expression does not occur in the snippet" in {
      Snippet("abc").indexOfFirst("d", before = 2, offset = 0) shouldBe Left(ExtractionError("No 'd' found"))
    }

    "return Left with ExtractionError if the given expression occurs but it's before the current snippet's startIdx" in {
      Snippet("abc").moveStartIdx(2).indexOfFirst("b", before = 2, offset = 0) shouldBe Left(ExtractionError("No 'b' found"))
    }

    "return Left with ExtractionError if the given expression occurs but before the given offset" in {
      Snippet("abc").indexOfFirst("b", before = 2, offset = 2) shouldBe Left(ExtractionError("No 'b' found"))
    }
  }

  "cutBetween" should {

    "move the snippet's window using given 'start' and 'end' expressions " +
      "so its startIdx points to the index next after end of the 'start' and its endIdx points to the index where the 'end' starts" in {
      Snippet("a<=smth==>b").cutBetween("<=", "==>") should (be('right) and have(startIdx(3)) and have(endIdx(7)))
    }

    "return ExtractionError if the start expression cannot be found" in {
      Snippet("a<=smth=>b").cutBetween("<==", "=>") shouldBe a[Left[_, _]]
    }

    "return ExtractionError if the end expression cannot be found" in {
      Snippet("a<=smth=>b").cutBetween("<=", "==>") shouldBe a[Left[_, _]]
    }
  }

  "cut" should {

    "move the snippet's window using given 'start' and 'end' expressions " +
      "so its startIdx points to the index where the 'start' starts and its endIdx points to the index next after the 'end'" in {
      Snippet("a<=smth==>b").cut("<=", "==>") should (be('right) and have(startIdx(1)) and have(endIdx(9)))
    }

    "return ExtractionError if the start expression cannot be found" in {
      Snippet("a<=smth=>b").cut("<==", "=>") shouldBe a[Left[_, _]]
    }

    "return ExtractionError if the end expression cannot be found" in {
      Snippet("a<=smth=>b").cut("<=", "==>") shouldBe a[Left[_, _]]
    }
  }

  "startsWith" should {

    "return true if the snippet's startIdx points to the given expression" in {
      Snippet("abc").moveStartIdx(1).startsWith("bc") shouldBe true
    }

    "return false if the snippet's startIdx does not point to the given expression" in {
      Snippet("abc").startsWith("bc") shouldBe false
    }
  }

  "endsWith" should {

    "return true if the snippet's endIdx points to the given expression" in {
      Snippet("abc").moveWindow(1, 2).endsWith("c") shouldBe true
    }

    "return false if the snippet's endIdx does not point to the given expression" in {
      Snippet("abc").endsWith("bc") shouldBe false
    }
  }

  "nonEmpty" should {

    "return true if the snippet's window is not a single point" in {
      Snippet("abc").moveWindow(1, 2).nonEmpty shouldBe true
    }

    "return false if the snippet's window is a single point" in {
      Snippet("abc").moveWindow(1, 1).nonEmpty shouldBe false
    }
  }
}
