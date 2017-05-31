package com.amdg.fhtml.types

import com.amdg.fhtml.tags.functions.SnippetOps

case class Snippet private(value: String, startIdx: Int, endIdx: Int)
  extends StringValue
    with NonEmpty
    with SnippetOps {

  require(startIdx > -1 && startIdx < value.length, "Snippet's startIdx cannot point outside the snippet")
  require(endIdx > -1 && endIdx < value.length, "Snippet's startIdx and endIdx cannot point outside the snippet")
  require(startIdx <= endIdx, "Snippet's startIdx has to be less than or equal endIdx")

  def moveStartIdx(newIdx: Int): Snippet = copy(
    startIdx = newIdx,
    endIdx = newIdx max endIdx
  )

  def moveStartIdxBy(v: Int): Snippet = copy(
    startIdx = startIdx + v,
    endIdx = startIdx + v max endIdx
  )

  def moveEndIdxBy(v: Int): Snippet = copy(
    startIdx = startIdx min endIdx + v,
    endIdx = endIdx + v
  )

  def moveWindow(startIdx: Int, endIdx: Int): Snippet = copy(
    startIdx = startIdx,
    endIdx = endIdx
  )

  override def toString = value.substring(startIdx, endIdx)
}

object Snippet {

  def apply(html: String): Snippet = Snippet(html, startIdx = 0, endIdx = 0)
}