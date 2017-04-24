package com.amdg.fhtml.functions

import cats.data.Reader

object Verifiers {

  type Condition = Reader[String, ExtractionError Either String]

  def verifyThat(snippet: String, condition: Condition): ExtractionError Either String = condition run snippet

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
  }

}
