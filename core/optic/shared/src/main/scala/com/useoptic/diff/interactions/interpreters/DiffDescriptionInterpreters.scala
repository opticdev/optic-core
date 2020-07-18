package com.useoptic.diff.interactions.interpreters

import com.useoptic.contexts.rfc.RfcState
import com.useoptic.contexts.shapes.Commands.{FieldId, ShapeId}
import com.useoptic.diff.ChangeType
import com.useoptic.diff.ChangeType.ChangeType
import com.useoptic.diff.interactions._
import com.useoptic.diff.interactions.interpreters.copy.InterpreterCopyHelper.Copy
import com.useoptic.diff.interactions.interpreters.copy.{NewBodiesTemplates, ShapeDiffTemplates}
import com.useoptic.diff.shapes._
import com.useoptic.diff.shapes.JsonTrailPathComponent._
import com.useoptic.diff.shapes.resolvers.JsonLikeResolvers
import com.useoptic.dsa.OpticDomainIds
import com.useoptic.types.capture.HttpInteraction
import com.useoptic.ux.ShapeNameRenderer

import scala.scalajs.js.annotation.{JSExport, JSExportAll}


sealed trait InteractionPointerDescription {
  def changeType: ChangeType
  def assertion: String
  def summary: Copy
  def path: Seq[String]
}

case class Unspecified(jsonTrail: JsonTrail, assertion: String, summary: Copy, path: Seq[String]) extends InteractionPointerDescription {
  def changeType: ChangeType = ChangeType.Addition
}

case class SpecifiedButNotMatching(jsonTrail: JsonTrail, shapeTrail: ShapeTrail, assertion: String, summary: Copy, path: Seq[String]) extends InteractionPointerDescription {
  def changeType: ChangeType = ChangeType.Update
}

case class SpecifiedButNotFound(jsonTrail: JsonTrail, shapeTrail: ShapeTrail, assertion: String, summary: Copy, path: Seq[String]) extends InteractionPointerDescription {
  def changeType: ChangeType = ChangeType.Removal
}

@JSExportAll
case class DiffDescription(titleCopy: Copy, assertion: String, interactionPointerDescription: Option[InteractionPointerDescription], changeType: ChangeType) {
  def changeTypeAsString: String = changeType.toString
  def summary: Copy = interactionPointerDescription.map(_.summary).getOrElse(Seq.empty)
  def path: Seq[String] = interactionPointerDescription.map(_.path).getOrElse(Seq.empty)
  def title: String = titleCopy.map(_.value).mkString(" ") //backwards compatible
}

@JSExport
@JSExportAll
class DiffDescriptionInterpreters(rfcState: RfcState)(implicit ids: OpticDomainIds) {
  private val namer = new ShapeNameRenderer(rfcState)
  def interpret(diff: ShapeDiffResult, inRequest: Boolean, interactionTrail: InteractionTrail, interaction: HttpInteraction): (Copy, InteractionPointerDescription) = {

    val inLocation = (if (inRequest) "Request" else s"${interactionTrail.statusCode()} Response") + " body"

    diff match {
      case UnspecifiedShape(jsonTrail, shapeTrail) => {
        val title = ShapeDiffTemplates.unspecifiedObserved(jsonTrailDescription(jsonTrail))
        val pointer = Unspecified(jsonTrail, jsonTrailAssertion(jsonTrail), ShapeDiffTemplates.newX(jsonTrailDetailedDescription(jsonTrail)), jsonTrailPathDescription(jsonTrail))
        (title, pointer)
      }
      case UnmatchedShape(jsonTrail, shapeTrail) => {
        val shapeDescription = expectedShapeDescription(shapeTrail)
        val bodyOption = JsonLikeResolvers.tryResolveJson(interactionTrail, jsonTrail, interaction)
        if (bodyOption.isEmpty) {
          val title = ShapeDiffTemplates.missing(jsonTrailDescription(jsonTrail))
          val pointer = SpecifiedButNotFound(jsonTrail, shapeTrail, s"required field is missing", ShapeDiffTemplates.missingRequired(jsonTrailDetailedDescription(jsonTrail)), jsonTrailPathDescription(jsonTrail))
          (title, pointer)
        } else {
          val title = ShapeDiffTemplates.wrongShape(jsonTrailDescription(jsonTrail), shapeDescription)
          val pointer = SpecifiedButNotMatching(jsonTrail, shapeTrail, s"expected a ${shapeDescription}", ShapeDiffTemplates.expectationNotMet(jsonTrailDetailedDescription(jsonTrail), shapeDescription), jsonTrailPathDescription((jsonTrail)))
          (title, pointer)
        }
      }
    }
  }

  def interpret(diff: InteractionDiffResult, interaction: HttpInteraction): DiffDescription = {
    diff match {
      case d: UnmatchedRequestUrl => {
        DiffDescription(NewBodiesTemplates.undocumented(interaction.request.method,interaction.request.path), "Not documented", None, ChangeType.Addition)
      }
      case d: UnmatchedRequestMethod => {
        DiffDescription(NewBodiesTemplates.undocumented(interaction.request.method,interaction.request.path), "Not documented", None, ChangeType.Addition)
      }
      case d: UnmatchedRequestBodyContentType => {
        ContentTypeHelpers.contentType(interaction.request) match {
          case Some(contentTypeHeader) => DiffDescription(NewBodiesTemplates.undocumentedContentType(contentTypeHeader), s"Not documented", None, ChangeType.Addition)
          case None => DiffDescription(NewBodiesTemplates.undocumentedNoBody(), s"Not documented", None, ChangeType.Addition)
        }
      }
      case d: UnmatchedRequestBodyShape => {
        val (shapeDiffDescription, pointerDescription) = interpret(d.shapeDiffResult, inRequest=true, d.interactionTrail, interaction)
        DiffDescription(shapeDiffDescription, pointerDescription.assertion, Some(pointerDescription), pointerDescription.changeType)
      }
      case d: UnmatchedResponseStatusCode => {
        DiffDescription(NewBodiesTemplates.undocumentedStatusCode(interaction.response.statusCode), s"Not documented", None, ChangeType.Addition)
      }
      case d: UnmatchedResponseBodyContentType => {
        ContentTypeHelpers.contentType(interaction.response) match {
          case Some(contentTypeHeader) => DiffDescription(NewBodiesTemplates.undocumentedContentType(contentTypeHeader), s"Not documented", None, ChangeType.Addition)
          case None => DiffDescription(NewBodiesTemplates.undocumentedNoBody(), s"Not documented", None, ChangeType.Addition)
        }
      }
      case d: UnmatchedResponseBodyShape => {
        val (shapeDiffDescription, pointerDescription) = interpret(d.shapeDiffResult, inRequest=false,  d.interactionTrail, interaction)
        DiffDescription(shapeDiffDescription, pointerDescription.assertion, Some(pointerDescription), pointerDescription.changeType)
      }
    }
  }

  def shapeName(shapeId: ShapeId): String = {
    val shape = rfcState.shapesState.shapes(shapeId)
    val name = shape.descriptor.name
    if (name.nonEmpty) {
      name
    } else {
      shapeName(shape.descriptor.baseShapeId)
    }
  }

  def expectedShapeDescription(shapeTrail: ShapeTrail) = shapeTrail.path.lastOption match {
    case Some(pc: UnknownTrail) => "Unknown"
    case Some(a: ShapeTrailPathComponent) => {
      val abc = (namer.nameForShapeId(a.namedShape))
      namer.nameForShapeId(a.namedShape).get.map(_.text).mkString(" ")
    } case None => {
      namer.nameForShapeId(shapeTrail.rootShapeId).get.map(_.text).mkString(" ")
    }
  }

  def jsonTrailDescription(jsonTrail: JsonTrail) = jsonTrail.path.lastOption match {
    case Some(value) => value match {
      case JsonObjectKey(key) => s"${key}"
      case JsonArrayItem(index) => s"shape at index ${index}"
      case _ => "shape?"
    }
    case None => "shape"
  }

  def jsonTrailDetailedDescription(jsonTrail: JsonTrail): Copy = {
    import com.useoptic.diff.interactions.interpreters.copy.InterpreterCopyHelper._
    jsonTrail.path.lastOption match {
      case Some(value) => value match {
        case JsonObjectKey(key) => Seq("field".t, key.code)
        case JsonArrayItem(index) => Seq("shape at index".t, index.toString.code)
        case _ => Seq("Shape".t)
      }
      case None => Seq("Shape".t)
    }
  }

  def jsonTrailPathDescription(jsonTrail: JsonTrail) : Seq[String] = jsonTrail.path.foldLeft(Seq.empty : Seq[String])((acc, pathComponent) => {
    val componentDescription = pathComponent match {
      case JsonObjectKey(key) => key
      case JsonArrayItem(index) => s"[${index}]"
      case _ => "shape?"
    }

    acc :+ componentDescription
  })

  def jsonTrailAssertion(jsonTrail: JsonTrail) = jsonTrail.path.lastOption match {
    case Some(value) => value match {
      case JsonObjectKey(key) => s"undocumented field"
      case JsonArrayItem(index) => s"undocumented shape at index ${index}"
      case _ => "undocumented"
    }
    case None => "undocumented"
  }

  def fieldName(fieldId: FieldId) = {
    rfcState.shapesState.fields(fieldId).descriptor.name
  }

}
