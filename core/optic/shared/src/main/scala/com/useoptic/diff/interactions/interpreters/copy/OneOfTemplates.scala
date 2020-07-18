package com.useoptic.diff.interactions.interpreters.copy

import com.useoptic.diff.interactions.interpreters.copy.InterpreterCopyHelper._

object OneOfTemplates {

  def makeOneOf(location: Copy, t1: String, t2: String): CopyPair =
    CopyPair(
      Seq("Allow".t) ++ location ++ Seq("to be either a".t, t1.code, "or".t, t2.code),
      Seq("Allowed".t) ++ location ++ Seq("to be either a".t, t1.code, "or".t, t2.code),
    )

  def addToOneOf(): CopyPair =
    CopyPair(
      Seq("Not Supported".t),
      Seq("Not Supported".t)
    )

}
