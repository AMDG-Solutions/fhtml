package com.amdg.fhtml.core.matchers

import com.amdg.fhtml.functions.ExtractionError
import com.amdg.fhtml.tags.Tag
import com.amdg.fhtml.tags.functions.TagError
import com.amdg.fhtml.types.Snippet
import org.scalatest.matchers.{HavePropertyMatchResult, HavePropertyMatcher}

object EitherMatchers {

  def tagName(name: String) = new HavePropertyMatcher[TagError Either Tag, String] {

    def apply(maybeTag: TagError Either Tag) = HavePropertyMatchResult(
      maybeTag.exists(_.tagName.value == name),
      "tagName",
      name,
      maybeTag.fold(
        error => s"Tag finding problem: $error",
        tag => tag.tagName.value
      )
    )
  }

  def startIdx(idx: Int) = new HavePropertyMatcher[ExtractionError Either Snippet, String] {

    def apply(maybeSnippet: ExtractionError Either Snippet) = HavePropertyMatchResult(
      maybeSnippet.exists(_.startIdx == idx),
      "startIdx",
      idx.toString,
      maybeSnippet.fold(
        error => s"Snippet processing problem: $error",
        snippet => snippet.startIdx.toString
      )
    )
  }

  def endIdx(idx: Int) = new HavePropertyMatcher[ExtractionError Either Snippet, String] {

    def apply(maybeSnippet: ExtractionError Either Snippet) = HavePropertyMatchResult(
      maybeSnippet.exists(_.endIdx == idx),
      "endIdx",
      idx.toString,
      maybeSnippet.fold(
        error => s"Snippet processing problem: $error",
        snippet => snippet.endIdx.toString
      )
    )
  }
}
