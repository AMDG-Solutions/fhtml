package com.amdg.fhtml.functions

import com.amdg.fhtml.core.LibSpec
import com.amdg.fhtml.core.matchers.EitherMatchers._
import com.amdg.fhtml.tags.{TagSnippet, TagName}
import com.amdg.fhtml.types.Snippet
import org.scalatest.prop.PropertyChecks

class ExtractorsSpec extends LibSpec with PropertyChecks {

  import Extractors._
  import Predicates._

  "extractFrom xxx next" should {

    "return Right with next token of the given type if the token is of that type" in {

      val snippet = Snippet("<abc></abc>")

      extractFrom(snippet, next[TagSnippet]) shouldBe TagSnippet.from(snippet.moveWindow(0, 4))
    }

    "return ExtractionError if the next token is not of the required type" in {

      val snippet = Snippet("abc<abc/>")

      extractFrom(snippet, next[TagSnippet]) shouldBe errorOfType[ExtractionError]
    }
  }

  "extractFrom xxx tagName" should {

    "return Right with tag name extracted from the given snippet " +
      "when there tag ends straight after tag name" in {
      TagSnippet.from(Snippet("<abc></abc>")) map { snippet =>
        extractFrom(snippet, tagName) shouldBe Right(TagName("abc"))
      }
    }

    "return Right with tag name extracted from the given snippet " +
      "when there are tag attributes" in {
      TagSnippet.from(Snippet("<abc attr></abc>")) map { snippet =>
        extractFrom(snippet, tagName) shouldBe Right(TagName("abc"))
      }
    }

    "return Right with tag name extracted from the given snippet " +
      "when tag is self-closing" in {
      TagSnippet.from(Snippet("<abc/>")) map { snippet =>
        extractFrom(snippet, tagName) shouldBe Right(TagName("abc"))
      }
    }

    "return Left with ExtractionError " +
      "when there is no tag name" in {
      TagSnippet.from(Snippet("<>")) map { snippet =>
        extractFrom(snippet, tagName) shouldBe Left(ExtractionError("No tag name found"))
      }
    }

    "return Left with ExtractionError " +
      "when there are whitespaces after '<'" in {
      TagSnippet.from(Snippet("< abc/>")) map { snippet =>
        extractFrom(snippet, tagName) shouldBe Left(ExtractionError("No tag name found"))
      }
    }

    "return Left with ExtractionError " +
      "when there is no tag name but just attributes" in {
      TagSnippet.from(Snippet("<abc=value />")) map { snippet =>
        extractFrom(snippet, tagName) shouldBe Left(ExtractionError("No tag name found"))
      }
    }
  }
}
