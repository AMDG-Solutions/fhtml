package com.amdg.fhtml.functions

import cats.data.Reader
import cats.syntax.either._
import com.amdg.fhtml.tags.{RawTag, TagName}

object Extractors {

  import StringOps._
  import Verifiers._
  import Conditions._

  def extractFrom[I, O](snippet: I,
                        predicate: Reader[I, ExtractionError Either O]): ExtractionError Either O = predicate run snippet

  object Predicates {

    val rawTag: Reader[String, ExtractionError Either RawTag] = Reader { snippet =>
      for {
        nonWhitespace <- snippet.trimWhitespaces
        _ <- verifyThat(nonWhitespace, startsWith("<")) leftMap (error => ExtractionError("no tag found"))
        tag <- snippet.cut(from = "<", to = ">") leftMap (_ => ExtractionError("no tag found"))
        tagContent <- tag.cutBetween("<", ">").flatMap(_.trimWhitespaces)
        _ <- verifyThat(tagContent, nonEmpty) leftMap (_ => ExtractionError("empty tag"))
      } yield RawTag(tag)
    }

    val tagName: Reader[RawTag, ExtractionError Either TagName] = Reader { tag =>

      val maybeTagName = for {
        tagEndIdx <- tag.value.indexOfFirst(">")
        _ <- verifyThat(tag.value, hasNoWhitespaceAt(idx = 1))
        tagNameEndIdx <- tag.value.indexOfFirst(" ", before = tagEndIdx, fromIdx = 1)
          .orElse(tag.value.indexOfFirst("/", before = tagEndIdx, fromIdx = 1))
          .orElse(tag.value.indexOfFirst(">", before = tagEndIdx + 1, fromIdx = 1))
        _ <- verifyThat(tag.value, hasNo("=", atOrBefore = tagNameEndIdx))
        tagName <- Right(tag.value.substring(1, tagNameEndIdx))
        _ <- verifyThat(tagName, nonEmpty) leftMap (_ => ExtractionError("tag name cannot be empty"))
      } yield TagName(tagName)

      maybeTagName leftMap (error => ExtractionError("No tag name found"))
    }
  }

}
