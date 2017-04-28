package com.amdg.fhtml.tags

import com.amdg.fhtml.tags.functions.TagFinders

trait Tag {
  val raw: RawTag
  val tagName: TagName
}

trait LeafTag extends Tag

trait NodeTag extends Tag with TagFinders