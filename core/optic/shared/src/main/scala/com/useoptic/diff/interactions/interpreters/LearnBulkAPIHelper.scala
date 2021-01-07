package com.useoptic.diff.interactions.interpreters

import com.useoptic.contexts.rfc.RfcState
import com.useoptic.diff.helpers.{UndocumentedUrlHelpers, UndocumentedUrlIncrementalHelpers}
import com.useoptic.diff.initial.{DistributionAwareShapeBuilder, ShapeBuildingStrategy, StreamingShapeBuilder}
import com.useoptic.diff.interactions.interpreters.ExpectedHelper.{expectedForDiff, parseShapeTrail}
import com.useoptic.diff.interactions.interpreters.LearnAPIHelper.ShapeBuilderMap
import com.useoptic.diff.interactions.{BodyUtilities, ContentTypeHelpers}
import com.useoptic.dsa.{OpticDomainIds, OpticIds}
import com.useoptic.types.capture.HttpInteraction
import io.circe.Json

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExport
@JSExportAll
case class EndpointToLearn(pathId: String, method: String)

@JSExport
@JSExportAll
object LearnBulkAPIHelper {
  def newLearner(rfcState: RfcState, json: Json, opticIds: OpticDomainIds = OpticIds.generator) = new LearnBulkAPIHelper(rfcState, json, opticIds)
}

@JSExportAll
class LearnBulkAPIHelper(rfcState: RfcState, json: Json, opticIds: OpticDomainIds = OpticIds.generator) {

  import io.circe._, io.circe.parser._
  import io.circe.generic.auto._
  import io.circe.syntax._

  val endpoints = json.as[Vector[EndpointToLearn]].right.get

  val shapeBuilderEndpointMaps: Map[EndpointToLearn, ShapeBuilderMap] = {
    endpoints.map(i => (i, LearnAPIHelper.newShapeBuilderMap(i.pathId, i.method, opticIds)))
  }.toMap

  val undocumentedUrlHelpers = new UndocumentedUrlIncrementalHelpers(rfcState)

  lazy val keySet = shapeBuilderEndpointMaps.keySet

  def interactionToEndpoint(interaction: HttpInteraction): Option[EndpointToLearn] = {
    val option = undocumentedUrlHelpers.tryResolvePathId(interaction.request.path).map(i => {
      EndpointToLearn(i, interaction.request.method)
    })
    if (option.isDefined && keySet.contains(option.get)) {
      option
    } else {
      None
    }
  }

  def learnBody(interaction: HttpInteraction): Unit = {
    interactionToEndpoint(interaction).foreach(endpoint => {
      LearnAPIHelper.learnBody(interaction, shapeBuilderEndpointMaps(endpoint))
    })
  }

  def getResults(endpointToLearn: EndpointToLearn): ShapeBuilderMap = {
    shapeBuilderEndpointMaps(endpointToLearn)
  }

  def getResults(pathId: String, method: String): ShapeBuilderMap = {
    shapeBuilderEndpointMaps.find(i => i._1.method == method && i._1.pathId == pathId).get._2
  }

}

