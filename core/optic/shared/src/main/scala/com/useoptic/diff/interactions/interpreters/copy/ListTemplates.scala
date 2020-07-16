package com.useoptic.diff.interactions.interpreters.copy

import com.useoptic.diff.interactions.interpreters.copy.InterpreterCopyHelper._

object ListTemplates {

  def setListItemShape(asType: String): CopyPair =
    CopyPair(
      Seq("Set the list item to ".t, asType.code),
      Seq("Assigned the list item to ".t, asType.code),
    )

  def addToOneOf(): CopyPair =
    CopyPair(
      Seq("Not Supported".t),
      Seq("Not Supported".t)
    )

}
