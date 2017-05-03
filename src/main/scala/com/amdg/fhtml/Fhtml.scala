package com.amdg.fhtml

import com.amdg.fhtml.tags.GenericTag
import com.amdg.fhtml.tags.functions.{TagError, TagFinders}

object Fhtml extends TagFinders {

  import TagFinders.Implicits._

  def apply(html: String): TagError Either GenericTag = find[GenericTag](html)

}
