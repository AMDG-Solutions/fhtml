package com.amdg.fhtml.types

import com.amdg.fhtml.core.LibSpec

class SnippetSpec extends LibSpec {

  "apply(html)" should {

    "instantiate a Snippet object with the window with start and end idx pointing to 0" in {
      Snippet("ab") should have(
        'startIdx (0),
        'endIdx (0)
      )
    }
  }

  "moveStartIdx" should {

    "move snippet's startIdx to the given index" in {
      Snippet("abc").moveWindow(0, 2).moveStartIdx(1) should have(
        'startIdx (1),
        'endIdx (2)
      )
    }

    "move snippet's startIdx and endIdx when both pointing to the same index" in {
      Snippet("abc").moveStartIdx(1) should have(
        'startIdx (1),
        'endIdx (1)
      )
    }

    "move snippet's startIdx and endIdx when endIdx before the new startIdx" in {

      val snippet = Snippet("abc").moveWindow(0, 1)

      snippet should have(
        'startIdx (0),
        'endIdx (1)
      )

      snippet.moveStartIdx(2) should have(
        'startIdx (2),
        'endIdx (2)
      )
    }

    "throw an exception if the given new startIdx is less min index" in {
      an[IllegalArgumentException] should be thrownBy Snippet("abc").moveStartIdx(-1)
    }

    "throw an exception if the given new startIdx is greater than max index" in {
      an[IllegalArgumentException] should be thrownBy Snippet("abc").moveStartIdx(3)
    }
  }

  "moveStartIdxBy" should {

    "move snippet's startIdx by the given positive value" in {
      Snippet("abc").moveWindow(0, 2).moveStartIdxBy(1) should have(
        'startIdx (1),
        'endIdx (2)
      )
    }

    "move snippet's startIdx by the given negative value" in {
      Snippet("abc").moveWindow(1, 2).moveStartIdxBy(-1) should have(
        'startIdx (0),
        'endIdx (2)
      )
    }

    "move snippet's startIdx and endIdx when both pointing to the same index" in {
      Snippet("abc").moveStartIdxBy(2) should have(
        'startIdx (2),
        'endIdx (2)
      )
    }

    "move snippet's startIdx and endIdx when endIdx before the new startIdx" in {

      val snippet = Snippet("abc").moveWindow(0, 1)

      snippet should have(
        'startIdx (0),
        'endIdx (1)
      )

      snippet.moveStartIdxBy(2) should have(
        'startIdx (2),
        'endIdx (2)
      )
    }

    "throw an exception if the given new startIdx is less min index" in {
      an[IllegalArgumentException] should be thrownBy Snippet("abc").moveStartIdxBy(-1)
    }

    "throw an exception if the given new startIdx is greater than max index" in {
      an[IllegalArgumentException] should be thrownBy Snippet("abc").moveStartIdxBy(3)
    }
  }

  "moveEndIdxBy" should {

    "move snippet's endIdx by the given positive value" in {
      Snippet("abc").moveWindow(0, 1).moveEndIdxBy(1) should have(
        'startIdx (0),
        'endIdx (2)
      )
    }

    "move snippet's endIdx by the given negative value" in {
      Snippet("abc").moveWindow(1, 2).moveEndIdxBy(-1) should have(
        'startIdx (1),
        'endIdx (1)
      )
    }

    "move snippet's startIdx and endIdx when both pointing to the same index" in {
      Snippet("abc").moveWindow(1, 1).moveEndIdxBy(-1) should have(
        'startIdx (0),
        'endIdx (0)
      )
    }

    "move snippet's startIdx and endIdx when startIdx after the new endIdx" in {

      val snippet = Snippet("abc").moveWindow(1, 2)

      snippet should have(
        'startIdx (1),
        'endIdx (2)
      )

      snippet.moveEndIdxBy(-2) should have(
        'startIdx (0),
        'endIdx (0)
      )
    }

    "throw an exception if the given new endIdx is less min index" in {
      an[IllegalArgumentException] should be thrownBy Snippet("abc").moveEndIdxBy(-1)
    }

    "throw an exception if the given new endIdx is greater than max index" in {
      an[IllegalArgumentException] should be thrownBy Snippet("abc").moveEndIdxBy(3)
    }
  }

  "moveWindow" should {

    "move snippet's startIdx and endIdx to the new indexes" in {
      Snippet("abc").moveWindow(1, 2) should have(
        'startIdx (1),
        'endIdx (2)
      )
    }

    "throw an exception if given start index is greater than the given end index" in {
      an[IllegalArgumentException] should be thrownBy Snippet("abc").moveWindow(2, 1)
    }

    "throw an exception if the given new startIdx is less min index" in {
      an[IllegalArgumentException] should be thrownBy Snippet("abc").moveWindow(-1, 1)
    }

    "throw an exception if the given new startIdx is greater than max index" in {
      an[IllegalArgumentException] should be thrownBy Snippet("abc").moveWindow(3, 1)
    }

    "throw an exception if the given new endIdx is less min index" in {
      an[IllegalArgumentException] should be thrownBy Snippet("abc").moveWindow(1, -1)
    }

    "throw an exception if the given new endIdx is greater than max index" in {
      an[IllegalArgumentException] should be thrownBy Snippet("abc").moveWindow(1, 3)
    }
  }

  "toString" should {

    "return an empty String if the window is a point" in {
      Snippet("ab").toString shouldBe ""
    }

    "return a String if the window is not a point" in {
      Snippet("a<=smth=>b").moveWindow(1, 9).toString shouldBe "<=smth=>"
    }
  }
}
