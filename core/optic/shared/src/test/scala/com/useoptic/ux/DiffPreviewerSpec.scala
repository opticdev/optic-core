package com.useoptic.ux

import com.useoptic.contexts.rfc.{RfcAggregate, RfcCommandContext, RfcService, RfcServiceJSFacade, RfcState}
import com.useoptic.contexts.shapes.ShapeEntity
import com.useoptic.contexts.shapes.ShapesHelper.ObjectKind
import com.useoptic.diff.initial.{DistributionAwareShapeBuilder, ShapeBuildingStrategy}
import com.useoptic.diff.interactions.{BodyUtilities, InteractionDiffResult}
import com.useoptic.diff.shapes.resolvers.DefaultShapesResolvers
import com.useoptic.diff.{ChangeType, DiffResult, JsonFileFixture}
import com.useoptic.diff.shapes.{JsonLikeAndSpecDiffVisitors, JsonLikeAndSpecTraverser, JsonLikeTraverser, JsonTrail, ShapeDiffResult, ShapeTrail, ShapeTraverser}
import com.useoptic.dsa.OpticIds
import com.useoptic.end_to_end.fixtures.{JsonExamples, ShapeExamples}
import com.useoptic.types.capture.{ArbitraryData, Body, JsonLikeFrom}
import io.circe.Json
import org.scalatest.FunSpec
import io.circe.generic.auto._
import io.circe.syntax._

class DiffPreviewerSpec extends FunSpec with JsonFileFixture {

  def rfcStateFromEvents(e: String): RfcState = {
    val events = eventsFrom(e)
    val rfcId: String = "rfc-1"
    val eventStore = RfcServiceJSFacade.makeEventStore()
    eventStore.append(rfcId, events)
    implicit val ids = OpticIds.newDeterministicIdGenerator
    val rfcService: RfcService = new RfcService(eventStore)
    rfcService.currentState(rfcId)
  }

  def diffPreview(shapeExample: (ShapeEntity, RfcState), observation: Json) = {
    val rfcState = shapeExample._2
    val resolvers = new DefaultShapesResolvers(rfcState)
    val previewDiffs = {
      val diffList = scala.collection.mutable.ListBuffer[ShapeDiffResult]()
      val visitor = new JsonLikeAndSpecDiffVisitors(resolvers, rfcState, (diff) => diffList.append(diff), _ => Unit)
      val traverse = new JsonLikeAndSpecTraverser(resolvers, rfcState, visitor)

      traverse.traverseRootShape(JsonLikeFrom.json(observation), shapeExample._1.shapeId)
      diffList.toVector
    }

    import com.useoptic.utilities.DistinctBy._
    val diffs = previewDiffs.distinctBy(i => i.shapeTrail).toSet
    new DiffPreviewer(resolvers, rfcState).previewDiff(JsonLikeFrom.json(observation), Some(ShapeExamples.todoShape._1.shapeId), diffs, previewDiffs.toSet)
  }

  def shapeOnlyPreview(shapeExample: (ShapeEntity, RfcState)) = {
    val rfcState = shapeExample._2
    val resolvers = new DefaultShapesResolvers(rfcState)
    new DiffPreviewer(resolvers, rfcState).previewShape(Some(ShapeExamples.todoShape._1.shapeId)).get
  }


  import RenderTester._
  
  it("shape only render") {
    val shapeOnly = shapeOnlyPreview(ShapeExamples.todoShape).getRootShape.get
    assert(shapeOnly.asInstanceOf[RenderSpecObject].fields.size == 2)
  }

  it("body only render") {
    val rfcState = RfcAggregate.initialState
    val resolvers = new DefaultShapesResolvers(rfcState)
    val bodyRender = new DiffPreviewer(resolvers, rfcState).previewBody(Body(Some("json"), ArbitraryData(asJsonString = Some(JsonExamples.basicTodo.noSpaces))))
    assert(bodyRender.isDefined)
  }

  it("render simulated spec json") {
    val rfcState = RfcAggregate.initialState
    val resolvers = new DefaultShapesResolvers(rfcState)
    val (commands, shapeOnly, rootShapeId) = new DiffPreviewer(resolvers, rfcState).shapeOnlyFromShapeBuilder(Vector(JsonLikeFrom.json(JsonExamples.basicTodo).get))(ShapeBuildingStrategy.inferPolymorphism).get
    assert(shapeOnly.specShapes.size == 3)
  }


  it("can render a string properly") {
    val rawBody = Body(Some("text/html"), ArbitraryData(
      shapeHashV1Base64 = Some("CAASUgpMPCFET0NUWVBFIGh0bWw+CjxodG1sPgo8aGVhZD4KICAgIDx0aXRsZT5uR2VuaXVzUFVMU0U8L3RpdGxlPgogICAgPGxpbmsgaHJlZhICCAISNQovY29weTsKICAgICAgICA8L2Rpdj4KICAgIDwvZGl2Pgo8L2Rpdj4KCjxkaXYgaWQSAggCElgKUiBtc2llIDwgMTEpIHsKCiAgICAgICAgICAgIGRvY3VtZW50LmdldEVsZW1lbnRCeUlkKCdiYWRCcm93c2VyRGl2Jykuc3R5bGUuZGlzcGxheSASAggC")
    ))

    val body = BodyUtilities.parseBody(rawBody)

    null

  }
}
