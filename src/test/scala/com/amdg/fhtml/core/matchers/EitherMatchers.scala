package com.amdg.fhtml.core.matchers

import com.amdg.fhtml.tags.Tag
import com.amdg.fhtml.tags.functions.TagError
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
}
