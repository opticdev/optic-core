package com.useoptic.diff.interactions.interpreters.copy

import com.useoptic.diff.interactions.interpreters.copy.InterpreterCopyHelper._

object OneOfTemplates {

  def makeOneOf(location: String, t1: String, t2: String): CopyPair =
    CopyPair(
      Seq("Allow".t, location.code, "to be either a".t, t1.code, "or".t, t2.code),
      Seq("Allowed".t, location.code, "to be either a".t, t1.code, "or".t, t2.code),
    )

  def addToOneOf(): CopyPair =
    CopyPair(
      Seq("Not Supported".t),
      Seq("Not Supported".t)
    )

}
