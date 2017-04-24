package com.amdg.fhtml.functions

import com.amdg.fhtml.core.LibSpec

class VerifiersSpec extends LibSpec {

  import Verifiers._
  import Conditions._

  "verifyThat xxx startsWith" should {

    "return Right with the given snippet " +
      "when it starts with the given expression" in {
      verifyThat("<abc/>", startsWith("<")) shouldBe Right("<abc/>")
    }

    "return Left with an ExtractionError " +
      "when the given snippet does not start with the expected expression" in {
      verifyThat("<abc/>", startsWith("a")) shouldBe Left(ExtractionError("'a' needs to be the first character"))
    }
  }

  "verifyThat xxx endsWith" should {

    "return Right with the given snippet " +
      "when it ends with the given expression" in {
      verifyThat("<abc/>", endsWith(">")) shouldBe Right("<abc/>")
    }

    "return Left with an ExtractionError " +
      "when the given snippet does not end with the expected expression" in {
      verifyThat("<abc/>", endsWith("/")) shouldBe Left(ExtractionError("'/' needs to be the last character"))
    }
  }
}
