package com.amdg.fhtml.functions

import cats.data.Reader
import cats.implicits._
import com.amdg.fhtml.types.NonEmptyString

object Extractors {

  type Predicate = Reader[String, ExtractionError Either String]

  def trimFrom(string: String,
               predicate: Predicate): ExtractionError Either String = predicate run string

  object Predicates {

    val allLeadingAndTrailingWhitespaces: Predicate = Reader { snippet =>
      Right(snippet.trim)
    }

    def allBetween(start: String,
                   end: String): Predicate = allBetween(NonEmptyString(start), NonEmptyString(end))

    def allBetween(start: NonEmptyString,
                   end: NonEmptyString): Predicate = Reader { snippet =>

      val NonEmptyString(startExpression) = start
      val NonEmptyString(endExpression) = end

      snippet.indexOfFirst(startExpression) |@| snippet.indexOfLast(endExpression) map {
        case (startIdx, endIdx) => snippet.substring(startIdx, endIdx + 1)
      }
    }

    private implicit class StringWrapper(snippet: String) {

      def indexOfFirst(str: String): ExtractionError Either Int = snippet.indexOf(str) match {
        case -1 => Left(ExtractionError(s"No '$str' found"))
        case idx => Right(idx)
      }

      def indexOfLast(str: String): ExtractionError Either Int = snippet.lastIndexOf(str) match {
        case -1 => Left(ExtractionError(s"No '$str' found"))
        case idx => Right(idx)
      }
    }

  }

}
