package com.amdg.fhtml.functions

import com.amdg.fhtml.core.LibSpec
import com.amdg.fhtml.tags.{RawTag, TagName}
import com.amdg.fhtml.types.Snippet
import org.scalatest.prop.PropertyChecks

class ExtractorsSpec extends LibSpec with PropertyChecks {

  import Extractors._
  import Predicates._

  "extractFrom xxx tagName" should {

    "return Right with tag name extracted from the given snippet " +
      "when there tag ends straight after tag name" in {
      RawTag.from(Snippet("<abc></abc>")) map { snippet =>
        extractFrom(snippet, tagName) shouldBe Right(TagName("abc"))
      }
    }

    "return Right with tag name extracted from the given snippet " +
      "when there are tag attributes" in {
      RawTag.from(Snippet("<abc attr></abc>")) map { snippet =>
        extractFrom(snippet, tagName) shouldBe Right(TagName("abc"))
      }
    }

    "return Right with tag name extracted from the given snippet " +
      "when tag is self-closing" in {
      RawTag.from(Snippet("<abc/>")) map { snippet =>
        extractFrom(snippet, tagName) shouldBe Right(TagName("abc"))
      }
    }

    "return Left with ExtractionError " +
      "when there is no tag name" in {
      RawTag.from(Snippet("<>")) map { snippet =>
        extractFrom(snippet, tagName) shouldBe Left(ExtractionError("No tag name found"))
      }
    }

    "return Left with ExtractionError " +
      "when there are whitespaces after '<'" in {
      RawTag.from(Snippet("< abc/>")) map { snippet =>
        extractFrom(snippet, tagName) shouldBe Left(ExtractionError("No tag name found"))
      }
    }

    "return Left with ExtractionError " +
      "when there is no tag name but just attributes" in {
      RawTag.from(Snippet("<abc=value />")) map { snippet =>
        extractFrom(snippet, tagName) shouldBe Left(ExtractionError("No tag name found"))
      }
    }
  }
}
