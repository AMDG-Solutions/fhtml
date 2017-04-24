package com.amdg.fhtml.functions

import com.amdg.fhtml.core.LibSpec
import com.amdg.fhtml.core.generators.StringGenerators._
import org.scalatest.prop.PropertyChecks

class ExtractorsSpec extends LibSpec with PropertyChecks {

  import Extractors._
  import Predicates._

  "trimFrom xxx allLeadingAndTrailingWhitespaces" should {

    "return Right the given snippet if there are no leading or trailing whitespaces" in {
      trimFrom("<abc></abc>", allLeadingAndTrailingWhitespaces) shouldBe Right("<abc></abc>")
    }

    "return Right the given snippet without leading or trailing whitespaces" in {

      forAll(whitespacesStrings) { whitespaces =>

        val snippet = s"$whitespaces<abc></abc>$whitespaces"

        trimFrom(snippet, allLeadingAndTrailingWhitespaces) shouldBe Right("<abc></abc>")
      }
    }

  }
}
