package com.amdg.fhtml

import com.amdg.fhtml.tags.GenericTag
import com.amdg.fhtml.tags.functions.{SnippetOps, TagError}
import com.amdg.fhtml.types.Snippet

object Fhtml {

  import SnippetOps.Implicits._

  def apply(html: String): TagError Either GenericTag = Snippet(html).as[GenericTag]

}
