package com.amdg.fhtml.tags

import com.amdg.fhtml.core.LibSpec
import com.amdg.fhtml.functions.ExtractionError
import com.amdg.fhtml.types.Snippet

class TagSnippetSpec extends LibSpec {

  "from" should {

    "return Right of TagSnippet if the given Snippet's window spans between '<' and '>'" in {

      val snippet = Snippet("a<tag>b").moveWindow(1, 5)

      TagSnippet.from(snippet) shouldBe 'right
    }

    "return Left with ExtractionError if the given Snippet window's startIdx does not point to '<'" in {

      val snippet = Snippet("a<tag>b").moveWindow(0, 5)

      val result = TagSnippet.from(snippet)

      result shouldBe 'left
      result.left.get shouldBe an[ExtractionError]
    }

    "return Left with ExtractionError if the given Snippet window's endIdx does not point to '>'" in {

      val snippet = Snippet("a<tag>b").moveWindow(1, 4)

      val result = TagSnippet.from(snippet)

      result shouldBe 'left
      result.left.get shouldBe an[ExtractionError]
    }

    "return Left with ExtractionError if the given Snippet window's is empty" in {

      val snippet = Snippet("a<>b").moveWindow(1, 2)

      val result = TagSnippet.from(snippet)

      result shouldBe 'left
      result.left.get shouldBe an[ExtractionError]
    }
  }
}

class TagBodySnippetSpec extends LibSpec {

  "from" should {

    "return Right of TagBodySnippet if the given Snippet's window spans after '>' and before '<'" in {

      val snippet = Snippet("<tag>b</tag>").moveWindow(5, 6)

      TagBodySnippet.from(snippet) shouldBe 'right
    }

    "return Right of TagBodySnippet if the given Snippet's window spans after '>' and at '<'" in {

      val snippet = Snippet("<tag></tag>").moveWindow(5, 5)

      TagBodySnippet.from(snippet) shouldBe 'right
    }

    "return Left with ExtractionError if the given Snippet window's startIdx does not point right after '>'" in {

      val snippet = Snippet("<tag>b</tag>").moveWindow(4, 6)

      val result = TagBodySnippet.from(snippet)

      result shouldBe 'left
      result.left.get shouldBe an[ExtractionError]
    }

    "return Left with ExtractionError if the given Snippet window's endIdx does not point right before '<'" in {

      val snippet = Snippet("<tag>b</tag>").moveWindow(5, 5)

      val result = TagBodySnippet.from(snippet)

      result shouldBe 'left
      result.left.get shouldBe an[ExtractionError]
    }
  }
}
