package com.amdg.fhtml.tags

import com.amdg.fhtml.functions.ExtractionError
import com.amdg.fhtml.functions.Verifiers.Conditions.{endsWith, nonEmptyBetween, startsWith}
import com.amdg.fhtml.functions.Verifiers.verifyThat
import com.amdg.fhtml.types.{NonEmpty, Snippet, StringValue}

case class TagSnippet private(value: Snippet)

object TagSnippet {

  def from(snippet: Snippet): ExtractionError Either TagSnippet = for {
    _ <- verifyThat(snippet, startsWith("<"))
    _ <- verifyThat(snippet, endsWith(">"))
    _ <- verifyThat(snippet, nonEmptyBetween("<", ">"))
  } yield TagSnippet(snippet)
}

case class TagName(value: String) extends StringValue with NonEmpty

case class TagBodySnippet private(value: Snippet)

object TagBodySnippet {

  def from(snippet: Snippet): ExtractionError Either TagBodySnippet = for {
    _ <- verifyThat(snippet.moveStartIdxBy(-1), startsWith(">"))
    _ <- verifyThat(snippet, endsWith("<"))
  } yield TagBodySnippet(snippet)
}