package com.amdg.fhtml.core.generators

import org.scalacheck.Gen

object StringGenerators {

  val whitespacesStrings = Gen.listOf(Gen.const(' ')).map(_.mkString)

}
