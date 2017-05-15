package com.amdg.fhtml.functions

import cats.data.Reader
import com.amdg.fhtml.types.Snippet

object Verifiers {

  import SnippetInternalOps._

  type Condition = Reader[Snippet, ExtractionError Either Snippet]

  def verifyThat(snippet: Snippet, condition: Condition): ExtractionError Either Snippet = condition run snippet

  object Conditions {

    def startsWith(expression: String): Condition = Reader { snippet =>
      Either.cond(
        test = snippet.startsWith(expression),
        right = snippet,
        left = ExtractionError(s"'$expression' needs to be the first character")
      )
    }

    def endsWith(expression: String): Condition = Reader { snippet =>
      Either.cond(
        test = snippet.endsWith(expression),
        right = snippet,
        left = ExtractionError(s"'$expression' needs to be the last character")
      )
    }

    lazy val nonEmpty: Condition = Reader { snippet =>
      Either.cond(
        test = snippet.nonEmpty,
        right = snippet,
        left = ExtractionError("Empty content")
      )
    }

    def nonEmptyBetween(start: String, end: String): Condition = Reader { snippet =>
      for {
        content <- snippet.cutBetween(start, end)
        _ <- Either.cond(
          test = content.nonEmpty,
          right = snippet,
          left = ExtractionError(s"Empty content between '$start' and '$end'")
        )
      } yield snippet
    }

    def hasNo(expression: String,
              atOrBefore: Int): Condition = Reader { snippet =>
      snippet.indexOfFirst(expression) match {
        case Right(expressionIdx) if expressionIdx <= atOrBefore => Left(ExtractionError(s"Found '$expression' when not expected"))
        case _ => Right(snippet)
      }
    }
  }

}
