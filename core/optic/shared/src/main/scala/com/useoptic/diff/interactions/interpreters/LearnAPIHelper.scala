package com.useoptic.diff.interactions.interpreters

import com.useoptic.contexts.requests.Commands.ShapedBodyDescriptor
import com.useoptic.contexts.rfc.Commands.RfcCommand
import com.useoptic.contexts.shapes.Commands.ShapeId
import com.useoptic.diff.initial.{DistributionAwareShapeBuilder, ShapeBuildingStrategy, StreamingShapeBuilder}
import com.useoptic.diff.interactions.{BodyUtilities, ContentTypeHelpers, InteractionDiffResult, UnmatchedRequestBodyContentType, UnmatchedResponseBodyContentType, UnmatchedResponseStatusCode}
import com.useoptic.dsa.OpticIds
import com.useoptic.contexts.requests.{RequestsServiceHelper, Commands => RequestsCommands}
import com.useoptic.types.capture.HttpInteraction

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExport
@JSExportAll
object LearnAPIHelper {
  private implicit val ids = OpticIds.generator
  private implicit val shapeBuilderStrategy = ShapeBuildingStrategy.inferPolymorphism


  def newShapeBuilder: StreamingShapeBuilder = DistributionAwareShapeBuilder.streaming
  def newShapeBuilderMap(pathId: String, method: String): ShapeBuilderMap = new ShapeBuilderMap(pathId: String, method: String)

  def learnBody(interaction: HttpInteraction, shapeBuilderMap: ShapeBuilderMap): Unit = {

    {
      val requestContentTypeOptional = ContentTypeHelpers.contentType(interaction.request)
      val shapeBuilder = shapeBuilderMap.getRequestOrPutDefault(requestContentTypeOptional)
      if (requestContentTypeOptional.isDefined) {
        BodyUtilities.parseBody(interaction.request.body).foreach(shapeBuilder.process)
      }
    }

    {
      val responseContentTypeOptional = ContentTypeHelpers.contentType(interaction.response)
      val shapeBuilder = shapeBuilderMap.getResponseOrPutDefault(interaction.response.statusCode, responseContentTypeOptional)
      if (responseContentTypeOptional.isDefined) {
        BodyUtilities.parseBody(interaction.response.body).foreach(shapeBuilder.process)
      }
    }

  }

  @JSExportAll
  case class ShapeBuilderResult(region: String, statusCode: Int, contentType: Option[String], rootShapeId: ShapeId, commands: Vector[RfcCommand])

  @JSExportAll
  class ShapeBuilderMap(val pathId: String, val method: String) {

    private val requestInteractionMap = scala.collection.mutable.Map[(Option[String]), StreamingShapeBuilder]()
    private val responseInteractionMap = scala.collection.mutable.Map[(Int, Option[String]), StreamingShapeBuilder]()

    def getRequestOrPutDefault(contentType: Option[String]): StreamingShapeBuilder = {
      requestInteractionMap.getOrElseUpdate((contentType), DistributionAwareShapeBuilder.streaming)
    }

    def getResponseOrPutDefault(statusCode: Int, contentType: Option[String]): StreamingShapeBuilder = {
      responseInteractionMap.getOrElseUpdate((statusCode, contentType), DistributionAwareShapeBuilder.streaming)
    }

    def requestRegions: Vector[ShapeBuilderResult] = {
      requestInteractionMap.collect {
        case ((contentType), shapeBuilder) => {
          val requestId = ids.newRequestId

          val optionShape = shapeBuilder.toCommandsOptional
          val (rootShapeId, flattenedShapeCommands) = if (optionShape.isDefined) {
            val (rootShapeId, shapeCommands) = optionShape.get
            (rootShapeId, shapeCommands.flatten)
          } else {
            ("!!empty-body!!", Vector.empty)
          }

          val baseCommands = Vector(
            RequestsCommands.AddRequest(requestId, pathId, method),
          )
          val commands = baseCommands ++ (if (contentType.isDefined) (flattenedShapeCommands ++ Vector(
            RequestsCommands.SetRequestBodyShape(requestId, ShapedBodyDescriptor(contentType.get, rootShapeId, isRemoved = false))
          )) else Vector.empty)

          ShapeBuilderResult("request", 0, contentType, rootShapeId, commands)
        }
      }.toVector
    }

    def responseRegions: Vector[ShapeBuilderResult] = {
      responseInteractionMap.collect {
        case ((statusCode, contentType), shapeBuilder) => {

          val optionShape = shapeBuilder.toCommandsOptional
          val (rootShapeId, flattenedShapeCommands) = if (optionShape.isDefined) {
            val (rootShapeId, shapeCommands) = optionShape.get
            (rootShapeId, shapeCommands.flatten)
          } else {
            ("!!empty-body!!", Vector.empty)
          }

          val responseId = ids.newResponseId

          val baseCommands = Vector(
            RequestsCommands.AddResponseByPathAndMethod(responseId, pathId, method, statusCode),
          )

          val commands = baseCommands ++ (if (contentType.isDefined) (flattenedShapeCommands ++ Vector(
            RequestsCommands.SetResponseBodyShape(responseId, ShapedBodyDescriptor(contentType.get, rootShapeId, isRemoved = false))
          )) else Vector.empty)

          ShapeBuilderResult("response", statusCode, contentType, rootShapeId, commands)
        }
      }.toVector
    }
  }

}

