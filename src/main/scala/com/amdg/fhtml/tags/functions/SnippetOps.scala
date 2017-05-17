package com.amdg.fhtml.tags.functions

import cats.data.Reader
import cats.implicits._
import com.amdg.fhtml.functions
import com.amdg.fhtml.tags.{GenericTag, Tag}
import com.amdg.fhtml.types.Snippet

object SnippetOps {

  private[tags] type TagFinder[T <: Tag] = Reader[Snippet, TagError Either T]

  object Implicits {

    import functions.Extractors.Predicates._
    import functions.Extractors._

    implicit val genericTagFinder: TagFinder[GenericTag] = Reader { snippet =>

      val maybeTag = for {
        rawTag <- extractFrom(snippet, next)
        tagName <- extractFrom(rawTag, tagName)
      } yield GenericTag(rawTag, tagName)

      maybeTag leftMap (error => GenericTagError(s"Given snippet is not valid html: $error", snippet))
    }
  }

}

trait SnippetOps {

  self: Snippet =>

  import SnippetOps._

  def as[T <: Tag](implicit tagFinder: TagFinder[T]): TagError Either T = tagFinder run self

}
