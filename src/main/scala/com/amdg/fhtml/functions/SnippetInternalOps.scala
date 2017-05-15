package com.amdg.fhtml.functions

import com.amdg.fhtml.types.Snippet

import scala.annotation.tailrec

private[functions] object SnippetInternalOps {

  implicit class SnippetPositionOps(snippet: Snippet) {

    lazy val trimLeadingWhitespaces: ExtractionError Either Snippet = {

      @tailrec
      def skipWhitespaces(from: Int): Int = snippet.value.charAt(from) match {
        case c if c <= ' ' => skipWhitespaces(from + 1)
        case _ => from
      }

      Right(snippet.moveStartIdx(skipWhitespaces(from = snippet.startIdx)))
    }

    def indexOfFirst(expression: String,
                     offset: Int = 0): ExtractionError Either Int =
      snippet.value.indexOf(expression, snippet.startIdx + offset) match {
        case -1 => Left(ExtractionError(s"No '$expression' found"))
        case idx => Right(idx)
      }

    def indexOfFirst(expression: String,
                     before: Int,
                     offset: Int): ExtractionError Either Int =
      snippet.value.indexOf(expression, snippet.startIdx + offset) match {
        case -1 => Left(ExtractionError(s"No '$expression' found"))
        case idx if idx >= before => Left(ExtractionError(s"No '$expression' found"))
        case idx => Right(idx)
      }

    def cutBetween(start: String, end: String): ExtractionError Either Snippet =
      for {
        startIdx <- indexOfFirst(start)
        endIdx <- indexOfFirst(end, offset = start.length)
      } yield snippet.moveWindow(startIdx + start.length, endIdx)

    def cut(from: String, to: String): ExtractionError Either Snippet =
      for {
        fromIdx <- indexOfFirst(from)
        toIdx <- indexOfFirst(to, offset = from.length)
      } yield snippet.moveWindow(fromIdx, toIdx + to.length - 1)

    def startsWith(expression: String): Boolean =
      snippet.value.startsWith(expression, snippet.startIdx)

    def endsWith(expression: String): Boolean =
      snippet.value.lastIndexOf(expression, snippet.endIdx) != -1

    lazy val nonEmpty: Boolean =
      snippet.startIdx != snippet.endIdx
  }

}
