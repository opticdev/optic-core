package com.useoptic.diff.interactions.interpreters.distribution_aware

import com.useoptic.contexts.requests.Commands.PathComponentId
import com.useoptic.diff.MutableCommandStream
import com.useoptic.diff.initial.{DistributionAwareShapeBuilder, FocusedStreamingShapeBuilder, ShapeBuildingStrategy, TrailValueMap, ValueAffordanceSerialization}
import com.useoptic.diff.interactions.{BodyUtilities, InteractionDiffResult, RequestSpecTrailHelpers}
import com.useoptic.dsa
import com.useoptic.dsa.OpticIds
import com.useoptic.serialization.CommandSerialization
import com.useoptic.types.capture.HttpInteraction
import io.circe.Json
import io.circe.parser.parse

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

  def toCommands(jsonString: String, deterministicIds: Boolean = false): LearnedCommands = {
    import io.circe._, io.circe.parser._
    import io.circe.generic.auto._
    import io.circe.syntax._
    implicit val ids = if (deterministicIds) OpticIds.newDeterministicIdGenerator else OpticIds.newRandomIdGenerator
    val results = parse(jsonString).right.get.as[Vector[ValueAffordanceSerialization]].right.get
    val trailmap = new TrailValueMap(ShapeBuildingStrategy.inferPolymorphism)(ids)
    trailmap.deserialize(results)
    val rootShape = DistributionAwareShapeBuilder.toShapes(trailmap)

    implicit val commands = new MutableCommandStream
    DistributionAwareShapeBuilder.buildCommandsFor(rootShape, None)
    LearnedCommands (rootShape.id, CommandSerialization.toJson(commands.toImmutable.flatten))

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
case class LearnedCommands(rootShapeId: String, commands: Json)
