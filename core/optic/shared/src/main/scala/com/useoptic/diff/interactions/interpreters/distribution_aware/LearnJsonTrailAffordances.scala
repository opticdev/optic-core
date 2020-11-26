package com.useoptic.diff.interactions.interpreters.distribution_aware

import com.useoptic.contexts.requests.Commands.PathComponentId
import com.useoptic.contexts.rfc.{RfcCommandContext, RfcService, RfcServiceJSFacade}
import com.useoptic.contexts.shapes.Commands.ShapeId
import com.useoptic.contexts.shapes.ShapesHelper
import com.useoptic.contexts.shapes.ShapesHelper.CoreShapeKind
import com.useoptic.diff.MutableCommandStream
import com.useoptic.diff.initial.{DistributionAwareShapeBuilder, FocusedStreamingShapeBuilder, ShapeBuildingStrategy, TrailValueMap, ValueAffordanceSerialization}
import com.useoptic.diff.interactions.{BodyUtilities, InteractionDiffResult, RequestSpecTrailHelpers}
import com.useoptic.diff.shapes.JsonTrail
import com.useoptic.dsa.{OpticDomainIds, OpticIds}
import com.useoptic.serialization.CommandSerialization
import com.useoptic.types.capture.HttpInteraction
import com.useoptic.ux.ShapeNameRenderer
import io.circe.Json

import scala.scalajs.js.annotation.{JSExport, JSExportAll}


@JSExportAll
@JSExport
object LearnJsonTrailAffordances {
  def newLearner(pathId: PathComponentId, method: String, diffs: Map[String, InteractionDiffResult]) =
    new LearnJsonTrailAffordances(pathId, method, diffs)
  def newLearner(pathId: PathComponentId, method: String, diffs: String) = {
    import io.circe._, io.circe.parser._
    import io.circe.generic.auto._
    import io.circe.syntax._
    new LearnJsonTrailAffordances(
      pathId,
      method,
      parse(diffs).right.get.as[Map[String,InteractionDiffResult]].right.get)
  }

  def toCommandsJson(valueAffordances: String, jsonTrailRaw: String, clientProvidedIdGenerator: OpticDomainIds, learnOnlyKind: Option[ShapeId]): Option[Json] = {
    import io.circe._, io.circe.parser._
    import io.circe.generic.auto._
    import io.circe.syntax._

    val limitToCoreShapeOption: Option[CoreShapeKind] =
      learnOnlyKind.flatMap(shapeId => ShapesHelper.allCoreShapes.find(i => i.baseShapeId == shapeId))

    val jsonTrail = parse(jsonTrailRaw).right.get.as[JsonTrail].right.get

    implicit val ids = clientProvidedIdGenerator
    val results = parse(valueAffordances).right.get.as[Vector[ValueAffordanceSerialization]].right.get
    val trailmap = new TrailValueMap(ShapeBuildingStrategy.inferPolymorphism)(ids)
    trailmap.deserialize(results)

    trailmap.getJsonTrail(jsonTrail).map(affordance => {
      val rootShape = affordance.toShape(limitToCoreShapeOption)


      implicit val commands = new MutableCommandStream
      DistributionAwareShapeBuilder.buildCommandsFor(rootShape, None)

      val finalCommands = commands.toImmutable.flatten


      val eventStore = RfcServiceJSFacade.makeEventStore()
      val simulatedId = "simulated-id"
      val rfcService = new RfcService(eventStore)
      rfcService.handleCommandSequence(simulatedId, finalCommands, RfcCommandContext("", "", ""))

      val currentState = rfcService.currentState(simulatedId)

      val namer = new ShapeNameRenderer(currentState)
      val flatName = namer.nameForShapeId(rootShape.id).get.map(_.text).mkString(" ").trim()

      LearnedCommands (rootShape.id, flatName, CommandSerialization.toJson(finalCommands))
    }).map(_.asJson)
  }

}

@JSExportAll
class LearnJsonTrailAffordances(val pathId: PathComponentId, val method: String, diffs: Map[String, InteractionDiffResult]) {

  require(diffs.forall(i => i._2.shapeDiffResultOption.isDefined), "Only Shape Diffs can have their values learned")

  private implicit val ids = OpticIds.generator
  private implicit val shapeBuilderStrategy = ShapeBuildingStrategy.inferPolymorphism

  val shapeBuilders = diffs.map(i => (i, new FocusedStreamingShapeBuilder(i._2.shapeDiffResultOption.get.jsonTrail)))

  def learnBody(interaction: HttpInteraction, interactionPointer: String): Unit = {
    shapeBuilders.foreach {case ((_, diff), shapeBuilder) => {
      val inRequest = RequestSpecTrailHelpers.requestId(diff.requestsTrail).isDefined
      val inResponse = RequestSpecTrailHelpers.responseId(diff.requestsTrail).isDefined

      if (inRequest) {
        BodyUtilities.parseBody(interaction.request.body).foreach(i => shapeBuilder.process(i, interactionPointer))
      }

      if (inResponse) {
        BodyUtilities.parseBody(interaction.response.body).foreach(i => shapeBuilder.process(i, interactionPointer))
      }
    }}
  }

  def serialize(): Json = {
    import io.circe.generic.auto._
    import io.circe.syntax._

    Json.fromFields(shapeBuilders.map {case ((diffHash, diff), shapeBuilder) => {
      diffHash -> shapeBuilder.serialize.asJson
    }})
  }

}


@JSExportAll
case class LearnedCommands(rootShapeId: String, name: String, commands: Json)
