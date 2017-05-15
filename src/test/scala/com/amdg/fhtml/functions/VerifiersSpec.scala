package com.amdg.fhtml.functions

import com.amdg.fhtml.core.LibSpec
import com.amdg.fhtml.core.matchers.EitherMatchers.{endIdx, startIdx}
import com.amdg.fhtml.types.Snippet

class VerifiersSpec extends LibSpec {

  import Verifiers._
  import Conditions._

  "verifyThat xxx startsWith" should {

    "return Right with the given snippet " +
      "when it starts with the given expression" in {
      verifyThat(Snippet("<abc/>"), startsWith("<")) shouldBe Right(Snippet("<abc/>"))
    }

    "return Left with an ExtractionError " +
      "when the given snippet does not start with the expected expression" in {
      verifyThat(Snippet("<abc/>"), startsWith("a")) shouldBe Left(ExtractionError("'a' needs to be the first character"))
    }
  }

  "verifyThat xxx endsWith" should {

    "return Right with the given snippet " +
      "when it ends with the given expression" in {
      verifyThat(Snippet("<abc/>").moveWindow(0, 5), endsWith(">")) should (be('right) and have(startIdx(0)) and have(endIdx(5)))
    }

    "return Left with an ExtractionError " +
      "when the given snippet does not end with the expected expression" in {
      verifyThat(Snippet("<abc/>"), endsWith("/")) shouldBe Left(ExtractionError("'/' needs to be the last character"))
    }
  }
}
