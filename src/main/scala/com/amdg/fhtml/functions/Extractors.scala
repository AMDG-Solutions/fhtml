package com.amdg.fhtml.functions

import cats.data.Reader
import cats.syntax.either._
import com.amdg.fhtml.tags.{TagSnippet, TagName}
import com.amdg.fhtml.types.Snippet

object Extractors {

  import SnippetInternalOps._
  import Verifiers._
  import Conditions._

  def extractFrom[I, O](snippet: I,
                        predicate: Reader[I, ExtractionError Either O]): ExtractionError Either O = predicate run snippet

  object Predicates {

    def next[T](implicit tokenFinder: Reader[Snippet, ExtractionError Either T]): Reader[Snippet, ExtractionError Either T] = Reader { snippet =>
      for {
        nonWhitespace <- snippet.trimLeadingWhitespaces
        tag <- tokenFinder.run(nonWhitespace)
      } yield tag
    }

    implicit val rawTagFinder: Reader[Snippet, ExtractionError Either TagSnippet] = Reader { snippet =>
      for {
        _ <- verifyThat(snippet, startsWith("<")) leftMap (error => ExtractionError(s"no tag found: $error"))
        tag <- snippet.cut(from = "<", to = ">") leftMap (_ => ExtractionError(s"no tag found: '${snippet.value}'; ${snippet.startIdx}; ${snippet.endIdx}"))
        tagContent <- tag.cutBetween("<", ">") flatMap (_.trimLeadingWhitespaces)
        _ <- verifyThat(tagContent, nonEmpty) leftMap (_ => ExtractionError("empty tag"))
      } yield TagSnippet(tag)
    }

    val tagName: Reader[TagSnippet, ExtractionError Either TagName] = Reader { tag =>

      val maybeTagName = for {
        tagEndIdx <- tag.value.indexOfFirst(">")
        tagNameEndIdx <- tag.value.indexOfFirst(" ", before = tagEndIdx, offset = 1)
          .orElse(tag.value.indexOfFirst("/", before = tagEndIdx, offset = 1))
          .orElse(tag.value.indexOfFirst(">", before = tagEndIdx + 1, offset = 1))
        _ <- verifyThat(tag.value, hasNo("=", atOrBefore = tagNameEndIdx))
        tagName <- Right(tag.value.moveWindow(tag.value.startIdx + 1, tagNameEndIdx))
        _ <- verifyThat(tagName, nonEmpty) leftMap (_ => ExtractionError("tag name cannot be empty"))
      } yield TagName(tagName.toString)

      maybeTagName leftMap (error => ExtractionError("No tag name found"))
    }
  }

}
