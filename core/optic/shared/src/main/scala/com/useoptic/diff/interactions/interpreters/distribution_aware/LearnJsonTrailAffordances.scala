package com.useoptic.diff.interactions.interpreters.distribution_aware

import com.useoptic.contexts.requests.Commands.PathComponentId
import com.useoptic.diff.initial.{FocusedStreamingShapeBuilder, ShapeBuildingStrategy}
import com.useoptic.diff.interactions.{BodyUtilities, InteractionDiffResult, RequestSpecTrailHelpers}
import com.useoptic.dsa.OpticIds
import com.useoptic.types.capture.HttpInteraction
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
      BodyUtilities.parseBody(interaction.request.body).foreach(i => shapeBuilder.process(i, interactionPointer))
    }

  }

  def serialize(): Json = {
    import io.circe.generic.auto._
    import io.circe.syntax._
    shapeBuilder.serialize.asJson
  }

}
