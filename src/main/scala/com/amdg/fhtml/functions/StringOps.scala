package com.amdg.fhtml.functions

private[functions] object StringOps {

  implicit class StringWrapper(snippet: String) {

    lazy val trimWhitespaces: ExtractionError Either String =
      Right(snippet.trim)

    def indexOfFirst(str: String,
                     fromIdx: Int = 0): ExtractionError Either Int =
      snippet.indexOf(str, fromIdx) match {
        case -1 => Left(ExtractionError(s"No '$str' found"))
        case idx => Right(idx)
      }

    def indexOfFirst(str: String,
                     before: Int,
                     fromIdx: Int): ExtractionError Either Int =
      snippet.indexOf(str, fromIdx) match {
        case -1 => Left(ExtractionError(s"No '$str' found"))
        case idx if idx >= before => Left(ExtractionError(s"No '$str' found"))
        case idx => Right(idx)
      }

    def indexOfLast(str: String): ExtractionError Either Int =
      snippet.lastIndexOf(str) match {
        case -1 => Left(ExtractionError(s"No '$str' found"))
        case idx => Right(idx)
      }

    def noWhitespaceAt(idx: Int): ExtractionError Either String =
      Either.cond(
        test = snippet.trim.charAt(idx) > ' ',
        right = snippet,
        left = ExtractionError(s"Whitespace found at idx: $idx when not expected")
      )

    def cutBetween(start: String, end: String): ExtractionError Either String =
      for {
        startIdx <- indexOfFirst(start)
        endIdx <- indexOfFirst(end, fromIdx = startIdx + start.length)
      } yield snippet.substring(startIdx + start.length, endIdx)

    def cut(from: String, to: String): ExtractionError Either String =
      for {
        fromIdx <- indexOfFirst(from)
        toIdx <- indexOfFirst(to, fromIdx = fromIdx + from.length)
      } yield snippet.substring(fromIdx, toIdx + 1)
  }

}
