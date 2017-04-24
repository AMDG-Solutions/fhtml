package com.amdg.fhtml.types

trait StringValue {

  def value: String

  override def toString = value
}

trait NonEmpty {

  self: StringValue =>

  require(value.nonEmpty, s"${this.getClass.getSimpleName} cannot be empty")
}