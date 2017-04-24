package com.amdg.fhtml.types

case class NonEmptyString(value: String) extends StringValue with NonEmpty
