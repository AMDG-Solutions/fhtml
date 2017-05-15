package com.amdg.fhtml.tags

import com.amdg.fhtml.functions.ExtractionError
import com.amdg.fhtml.functions.Verifiers.Conditions.{endsWith, nonEmptyBetween, startsWith}
import com.amdg.fhtml.functions.Verifiers.verifyThat
import com.amdg.fhtml.types.{NonEmpty, Snippet, StringValue}

case class RawTag private(value: Snippet)

object RawTag {

  def from(snippet: Snippet): ExtractionError Either RawTag = for {
    _ <- verifyThat(snippet, startsWith("<"))
    _ <- verifyThat(snippet, endsWith(">"))
    _ <- verifyThat(snippet, nonEmptyBetween("<", ">"))
  } yield RawTag(snippet)
}

case class TagName(value: String) extends StringValue with NonEmpty
