package com.amdg.fhtml.tags

import com.amdg.fhtml.functions.ExtractionError
import com.amdg.fhtml.functions.Verifiers.Conditions.{endsWith, nonEmptyBetween, startsWith}
import com.amdg.fhtml.functions.Verifiers.verifyThat
import com.amdg.fhtml.types.{NonEmpty, StringValue}

case class RawTag private(value: String) extends StringValue with NonEmpty

object RawTag {
  def from(snippet: String): ExtractionError Either RawTag = for {
    _ <- verifyThat(snippet, startsWith("<"))
    _ <- verifyThat(snippet, endsWith(">"))
    _ <- verifyThat(snippet, nonEmptyBetween("<", ">"))
  } yield RawTag(snippet)
}

case class TagName(value: String) extends StringValue with NonEmpty
