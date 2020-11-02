package com.useoptic.diff.interactions.interpreters.distribution_aware

import com.useoptic.contexts.requests.Commands.PathComponentId
import com.useoptic.contexts.rfc.{RfcCommandContext, RfcService, RfcServiceJSFacade}
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
  def newLearner(pathId: PathComponentId, method: String, diff: InteractionDiffResult) =
    new LearnJsonTrailAffordances(pathId, method, diff)
  def newLearner(pathId: PathComponentId, method: String, diff: String) = {

    import io.circe._, io.circe.parser._
    import io.circe.generic.auto._
    import io.circe.syntax._

    new LearnJsonTrailAffordances(
      pathId,
      method,
      parse(diff).right.get.as[InteractionDiffResult].right.get)
  }

  def toCommandsJson(valueAffordances: String, jsonTrailRaw: String, clientProvidedIdGenerator: OpticDomainIds): Option[Json] = {
    import io.circe._, io.circe.parser._
    import io.circe.generic.auto._
    import io.circe.syntax._

    val jsonTrail = parse(jsonTrailRaw).right.get.as[JsonTrail].right.get

    implicit val ids = clientProvidedIdGenerator
    val results = parse(valueAffordances).right.get.as[Vector[ValueAffordanceSerialization]].right.get
    val trailmap = new TrailValueMap(ShapeBuildingStrategy.inferPolymorphism)(ids)
    trailmap.deserialize(results)

    trailmap.getJsonTrail(jsonTrail).map(affordance => {
      val rootShape = affordance.toShape
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
class LearnJsonTrailAffordances(val pathId: PathComponentId, val method: String, diff: InteractionDiffResult) {

  require(diff.shapeDiffResultOption.isDefined, "Only Shape Diffs can have their values learned")

  private implicit val ids = OpticIds.generator
  private implicit val shapeBuilderStrategy = ShapeBuildingStrategy.inferPolymorphism

  val shapeBuilder: FocusedStreamingShapeBuilder = new FocusedStreamingShapeBuilder(diff.shapeDiffResultOption.get.jsonTrail)

  def learnBody(interaction: HttpInteraction, interactionPointer: String): Unit = {

    val inRequest = RequestSpecTrailHelpers.requestId(diff.requestsTrail).isDefined
    val inResponse = RequestSpecTrailHelpers.responseId(diff.requestsTrail).isDefined

    if (inRequest) {
      BodyUtilities.parseBody(interaction.request.body).foreach(i => shapeBuilder.process(i, interactionPointer))
    }

    if (inResponse) {
      BodyUtilities.parseBody(interaction.response.body).foreach(i => shapeBuilder.process(i, interactionPointer))
    }

  }

  def serialize(): Json = {
    import io.circe.generic.auto._
    import io.circe.syntax._
    shapeBuilder.serialize.asJson
  }

}


@JSExportAll
case class LearnedCommands(rootShapeId: String, name: String, commands: Json)
