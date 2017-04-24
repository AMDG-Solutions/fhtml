package com.amdg.fhtml

import com.amdg.fhtml.core.LibSpec
import com.amdg.fhtml.core.generators.StringGenerators._
import com.amdg.fhtml.tags.GenericTag
import com.amdg.fhtml.tags.functions.GenericTagError
import org.scalatest.prop.PropertyChecks

class FhtmlSpec extends LibSpec with PropertyChecks {

  "apply" should {

    "return Right with an instance of GenericTag if the given html is valid" in {
      Fhtml("<html></html>") shouldBe Right(GenericTag("<html></html>"))
    }

    "return Right with an instance of GenericTag containing the given html " +
      "when the given snippet contain leading and trailing whitespaces" in {
      forAll(whitespacesStrings) { whitespaces =>
        Fhtml(s"$whitespaces<html></html>$whitespaces") shouldBe Right(GenericTag("<html></html>"))
      }
    }

    "return Left with a TagError " +
      "if the given html does not contain any tag" in {
      Fhtml("abc") shouldBe Left(GenericTagError("Given snippet is not valid html: '<' needs to be the first character", "abc"))
    }

    "return Left with a TagError " +
      "if the given html starts with non whitespace characters" in {
      Fhtml("a<abc/>") shouldBe Left(GenericTagError("Given snippet is not valid html: '<' needs to be the first character", "a<abc/>"))
    }

    "return Left with a TagError " +
      "if the given html ends with non whitespace characters" in {
      Fhtml("<abc/>a") shouldBe Left(GenericTagError("Given snippet is not valid html: '>' needs to be the last character", "<abc/>a"))
    }
  }
}
