package com.useoptic.diff.interactions.interpreters.copy

import com.useoptic.diff.interactions.interpreters.copy.InterpreterCopyHelper._

object NewBodiesTemplates {


}

object NewBodiesSuggestionTemplates {
//
  def addResponseType(statusCode: Int, contentType: Option[String], hasBody: Boolean): CopyPair =
    CopyPair(
      Seq("Add".t, statusCode.toString.code, "Response with".t, if (contentType.isDefined) contentType.get.code else "No".t, "Content-Type".t, (if (!hasBody) "and no body" else "").t),
      Seq("Added".t, statusCode.toString.code, "Response with".t, if (contentType.isDefined) contentType.get.code else "No".t, "Content-Type".t, (if (!hasBody) "and no body" else "").t),
    )

  def addRequestType(contentType: String, hasBody: Boolean): CopyPair =
    CopyPair(
      Seq("Add Request with".t, contentType.code, "Content-Type".t, (if (!hasBody) "and no body" else "").t),
      Seq("Added Request with".t, contentType.code, "Content-Type".t, (if (!hasBody) "and no body" else "").t),
    )

  def addRequestNoContentOrBody(): CopyPair =
    CopyPair(
      Seq("Add Request with No Body".t),
      Seq("Added Request with No Body".t)
    )


  @deprecated
  def addRequestNoBody(): CopyPair =
    CopyPair(
      Seq("Add Request with No Body".t),
      Seq("Added Request with No Body".t),
    )

  def changeShape(fieldName: String, toShape: String) = Seq("Change field", fieldName.code, "to", toShape.code)
  def makeFieldOptional(fieldName: String, asType: String) = Seq("Make field", fieldName.code, "optional")
  def removeField(fieldName: String) = Seq("Remove field", fieldName.code)

}
