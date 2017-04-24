package com.amdg.fhtml.tags.functions

import com.amdg.fhtml.FhtmlError

sealed trait TagError extends FhtmlError {
  val message: String
  val rawHtml: String
}

case class GenericTagError(message: String, rawHtml: String) extends TagError
