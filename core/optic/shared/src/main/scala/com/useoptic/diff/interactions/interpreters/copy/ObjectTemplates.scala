package com.useoptic.diff.interactions.interpreters.copy
import InterpreterCopyHelper._

object ObjectDiffTemplates {

  def newField(fieldName: String) = Seq("New field observed:", fieldName.code)
  def requiredFieldIsMissing(fieldName: String) = Seq("Required field is missing:", fieldName.code)
  def requiredFieldIsNull(fieldName: String) = Seq("Required field is null:", fieldName.code)

  def wrongShape(fieldName: String, expectedType: String, actualType: String) =
    Seq("Expected", expectedType.code, "for field:", fieldName.code, "observed", actualType.code)

}

object ObjectSuggestionTemplates {
//
  def addField(fieldName: String, asType: String): CopyPair = CopyPair(
    Seq("Add field".t, fieldName.code, "as".t, asType.code),
    Seq("Added field".t, fieldName.code, "as".t, asType.code),
  )

  def changeShape(fieldName: String, toShape: String) = Seq("Change field", fieldName.code, "to", toShape.code)

  def makeFieldOptional(fieldName: String) = {
    CopyPair(
      Seq("Make field".t, fieldName.code, "optional".t),
      Seq("Make field".t, fieldName.code, "optional".t)
    )
  }
  def removeField(fieldName: String) = {
    CopyPair(
      Seq("Remove field".text, fieldName.code),
      Seq("Removed field".text, fieldName.code),
    )
  }

}
