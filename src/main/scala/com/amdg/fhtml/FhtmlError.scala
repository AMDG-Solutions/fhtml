package com.amdg.fhtml

trait FhtmlError {

  def message: String

  override def toString = message
}
