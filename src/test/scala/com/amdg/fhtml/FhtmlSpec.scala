package com.amdg.fhtml

import com.amdg.fhtml.core.LibSpec
import com.amdg.fhtml.core.generators.StringGenerators._
import com.amdg.fhtml.core.matchers.EitherMatchers._
import com.amdg.fhtml.tags.functions.GenericTagError
import org.scalatest.prop.PropertyChecks

class FhtmlSpec extends LibSpec with PropertyChecks {

  "apply" should {

    "return Right with an instance of GenericTag " +
      "if the given snippet starts with valid 'html' opening tag" in {
      Fhtml("<html>abc</html>") should (be('right) and have(tagName("html")))
    }

    "return Right with an instance of GenericTag " +
      "if the given snippet starts with a valid self-closing tag" in {
      Fhtml("<tag abc />") should (be('right) and have(tagName("tag")))
    }

    "return Right with an instance of GenericTag " +
      "if the given snippet starts with some valid opening tag" in {
      Fhtml("<tag>") should (be('right) and have(tagName("tag")))
    }

    "return Right with an instance of GenericTag " +
      "if the given snippet starts with some valid opening tag prepended with some whitespaces" in {
      forAll(whitespacesStrings) { whitespaces =>
        Fhtml(s"$whitespaces<tag>") should (be('right) and have(tagName("tag")))
      }
    }

    "return Right with an instance of GenericTag " +
      "if there are some chars after closing tag" in {
      Fhtml("<tag/>a") should (be('right) and have(tagName("tag")))
    }

    "return Left with a TagError " +
      "if the given html does not contain any tag" in {
      Fhtml("abc") shouldBe Left(GenericTagError("Given snippet is not valid html: no tag found", "abc"))
    }

    "return Left with a TagError " +
      "if the given html starts with non whitespace characters" in {
      Fhtml("a<abc/>") shouldBe Left(GenericTagError("Given snippet is not valid html: no tag found", "a<abc/>"))
    }

    "return Left with a TagError " +
      "if there is opening tag is not closed" in {
      Fhtml("<a") shouldBe Left(GenericTagError("Given snippet is not valid html: no tag found", "<a"))
    }

    "return Left with a TagError " +
      "if tag is empty" in {
      forAll(whitespacesStrings) { whitespaces =>
        val emptyTag = s"<$whitespaces>"
        Fhtml(emptyTag) shouldBe Left(GenericTagError("Given snippet is not valid html: empty tag", emptyTag))
      }
    }
  }
}
