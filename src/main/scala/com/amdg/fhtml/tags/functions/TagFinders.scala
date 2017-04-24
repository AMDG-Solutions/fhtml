package com.amdg.fhtml.tags.functions

import cats.data.Reader
import cats.syntax.either._
import com.amdg.fhtml.tags.{GenericTag, Tag}
import com.amdg.fhtml.functions

object TagFinders extends TagFinders {

  private[tags] type TagFinder[T <: Tag] = Reader[String, TagError Either T]

  object Implicits {

    import functions.Extractors._
    import functions.Extractors.Predicates._
    import functions.Verifiers._
    import functions.Verifiers.Conditions._

    implicit val genericTagFinder: TagFinder[GenericTag] = Reader { html =>
      (for {
        maybeTag <- trimFrom(html, allLeadingAndTrailingWhitespaces)
        _ <- verifyThat(maybeTag, startsWith("<"))
        _ <- verifyThat(maybeTag, endsWith(">"))
      } yield GenericTag(maybeTag)).leftMap(error => GenericTagError(s"Given snippet is not valid html: $error", html))
    }
  }

}

trait TagFinders {

  import TagFinders._

  def findTag(html: String)
             (implicit tagFinder: TagFinder[GenericTag]): TagError Either GenericTag = tagFinder.run(html)

}
