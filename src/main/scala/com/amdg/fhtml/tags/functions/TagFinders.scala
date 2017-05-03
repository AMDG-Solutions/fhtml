package com.amdg.fhtml.tags.functions

import cats.data.Reader
import cats.implicits._
import com.amdg.fhtml.functions
import com.amdg.fhtml.tags.{GenericTag, Tag}

object TagFinders extends TagFinders {

  private[tags] type TagFinder[T <: Tag] = Reader[String, TagError Either T]

  object Implicits {

    import functions.Extractors.Predicates._
    import functions.Extractors._

    implicit val genericTagFinder: TagFinder[GenericTag] = Reader { snippet =>

      val maybeTag = for {
        rawTag <- extractFrom(snippet, rawTag)
        tagName <- extractFrom(rawTag, tagName)
      } yield GenericTag(rawTag, tagName)

      maybeTag leftMap (error => GenericTagError(s"Given snippet is not valid html: $error", snippet))
    }
  }

}

trait TagFinders {

  import TagFinders._

  def find[T <: Tag](html: String)
                    (implicit tagFinder: TagFinder[T]): TagError Either T = tagFinder run html

}
