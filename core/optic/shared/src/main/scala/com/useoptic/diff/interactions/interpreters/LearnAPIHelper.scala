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

    val requestContentTypeOptional = ContentTypeHelpers.contentType(interaction.request)
    if (requestContentTypeOptional.isDefined) {
      val shapeBuilder = shapeBuilderMap.getRequestOrPutDefault(requestContentTypeOptional.get)
      BodyUtilities.parseBody(interaction.request.body).foreach(shapeBuilder.process)
    }

    val responseContentTypeOptional = ContentTypeHelpers.contentType(interaction.response)
    if (responseContentTypeOptional.isDefined) {
      val shapeBuilder = shapeBuilderMap.getResponseOrPutDefault(interaction.response.statusCode, responseContentTypeOptional.get)
      BodyUtilities.parseBody(interaction.response.body).foreach(shapeBuilder.process)
    }

  }

  @JSExportAll
  case class ShapeBuilderResult(region: String, statusCode: Int, contentType: String, rootShapeId: ShapeId, commands: Vector[RfcCommand])

  @JSExportAll
  class ShapeBuilderMap(val pathId: String, val method: String) {

    private val requestInteractionMap = scala.collection.mutable.Map[(String), StreamingShapeBuilder]()
    private val responseInteractionMap = scala.collection.mutable.Map[(Int, String), StreamingShapeBuilder]()

    def getRequestOrPutDefault(contentType: String): StreamingShapeBuilder = {
      requestInteractionMap.getOrElseUpdate((contentType), DistributionAwareShapeBuilder.streaming)
    }

    def getResponseOrPutDefault(statusCode: Int, contentType: String): StreamingShapeBuilder = {
      responseInteractionMap.getOrElseUpdate((statusCode, contentType), DistributionAwareShapeBuilder.streaming)
    }

    def requestRegions: Vector[ShapeBuilderResult] = {
      requestInteractionMap.collect {
        case ((contentType), shapeBuilder) => {
          val requestId = ids.newRequestId
          val (rootShapeId, shapeCommands) = shapeBuilder.toCommands
          val flattenedShapeCommands = shapeCommands.flatten
          val baseCommands = Vector(
            RequestsCommands.AddRequest(requestId, pathId, method),
          )
          val commands = baseCommands ++ flattenedShapeCommands ++ Vector(
            RequestsCommands.SetRequestBodyShape(requestId, ShapedBodyDescriptor(contentType, rootShapeId, isRemoved = false))
          )

          ShapeBuilderResult("request", 0, contentType, rootShapeId, commands)
        }
      }.toVector
    }

    def responseRegions: Vector[ShapeBuilderResult] = {
      responseInteractionMap.collect {
        case ((statusCode, contentType), shapeBuilder) => {
          val (rootShapeId, shapeCommands) = shapeBuilder.toCommands
          val flattenedShapeCommands = shapeCommands.flatten
          val responseId = ids.newResponseId

          val baseCommands = Vector(
            RequestsCommands.AddResponseByPathAndMethod(responseId, pathId, method, statusCode),
          )

          val commands = baseCommands ++ flattenedShapeCommands ++ Vector(
            RequestsCommands.SetResponseBodyShape(responseId, ShapedBodyDescriptor(contentType, rootShapeId, isRemoved = false))
          )

          ShapeBuilderResult("response", statusCode, contentType, rootShapeId, commands)
        }
      }.toVector
    }
  }

}

