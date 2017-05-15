package com.amdg.fhtml.tags

trait Tag {
  val raw: RawTag
  val tagName: TagName
}

trait LeafTag extends Tag

trait NodeTag extends Tag