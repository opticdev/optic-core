package com.useoptic.diff.interactions.interpreters

import com.useoptic.contexts.rfc.RfcState
import com.useoptic.diff.helpers.{UndocumentedUrlHelpers, UndocumentedUrlIncrementalHelpers}
import com.useoptic.diff.initial.{DistributionAwareShapeBuilder, ShapeBuildingStrategy, StreamingShapeBuilder}
import com.useoptic.diff.interactions.interpreters.LearnAPIHelper.ShapeBuilderMap
import com.useoptic.diff.interactions.{BodyUtilities, ContentTypeHelpers}
import com.useoptic.dsa.{OpticDomainIds, OpticIds}
import com.useoptic.types.capture.HttpInteraction

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

case class EndpointToLearn(pathId: String, method: String)

@JSExport
@JSExportAll
class LearnBulkAPIHelper(rfcState: RfcState, endpoints: Vector[EndpointToLearn], opticIds: OpticDomainIds = OpticIds.generator) {

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

}

