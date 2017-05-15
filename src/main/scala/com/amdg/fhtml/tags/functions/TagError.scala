package com.amdg.fhtml.tags.functions

import com.amdg.fhtml.FhtmlError
import com.amdg.fhtml.types.Snippet

sealed trait TagError extends FhtmlError {
  val message: String
  val rawHtml: Snippet
}

case class GenericTagError(message: String, rawHtml: Snippet) extends TagError
