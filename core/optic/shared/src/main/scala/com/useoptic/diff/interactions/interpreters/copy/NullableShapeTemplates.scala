package com.useoptic.diff.interactions.interpreters.copy

import com.useoptic.diff.interactions.interpreters.copy.InterpreterCopyHelper._

object NullableShapeTemplates {

  def assignNullable(toType: String): CopyPair =
    CopyPair(
      Seq("Replace".t, "Unknown (nullable)".code, " with ".t, (toType+ " (nullable)").code),
      Seq("Replaced".t, "Unknown (nullable)".code, " with ".t, (toType+ " (nullable)").code),
    )

  def makeNullable(location: String): CopyPair =
    CopyPair(
      Seq("Make".t, location.code, "nullable".text),
      Seq("Made".t, location.code, "nullable".text),
    )

}
