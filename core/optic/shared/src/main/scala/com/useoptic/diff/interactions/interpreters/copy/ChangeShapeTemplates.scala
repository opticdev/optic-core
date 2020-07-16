package com.useoptic.diff.interactions.interpreters.copy

import com.useoptic.diff.interactions.interpreters.copy.InterpreterCopyHelper._

object ChangeShapeSuggestionTemplates {

  def changeShape(location: String, toType: String): CopyPair =
    CopyPair(
      Seq("Change shape to ".t, toType.code),
      Seq("Changed shape of ".t, location.code, "to".t, toType.code),
    )

}
