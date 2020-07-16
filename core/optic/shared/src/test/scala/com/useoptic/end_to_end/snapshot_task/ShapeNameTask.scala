package com.useoptic.end_to_end.snapshot_task

import com.useoptic.contexts.rfc.Events.RfcEvent
import com.useoptic.contexts.rfc.{RfcService, RfcServiceJSFacade}
import com.useoptic.diff.MutableCommandStream
import com.useoptic.diff.initial.{DistributionAwareShapeBuilder, ShapeBuildingStrategy}
import com.useoptic.dsa.OpticIds
import com.useoptic.end_to_end.snapshot_task.ShapeNameTask.Input
import com.useoptic.serialization.{CommandSerialization, EventSerialization, InteractionSerialization}
import com.useoptic.types.capture.JsonLikeFrom
import com.useoptic.ux.ShapeNameRenderer
import io.circe.{Json, JsonObject}
import io.circe.generic.auto._
import io.circe.syntax._

object ShapeNameTask {
  case class Input(events: Vector[RfcEvent], shapeId: String)
  case class Output(flatStringName: String, renderComponents: Vector[String])
}

class ShapeNameTask
  extends SnapShotDriverFixture[ShapeNameTask.Input, ShapeNameTask.Output]("naming-shapes", "Naming Shapes") {
  override def serializeOutput(output: ShapeNameTask.Output): Json = output.asJson

  override def deserializeInput(json: Json): ShapeNameTask.Input = {
      val events = json.asObject.get("events").get
      Input(EventSerialization.fromJson(events).get, json.asObject.get("shapeId").asJson.asString.get)
  }

  override def serializeInput(input: ShapeNameTask.Input): Json = {
    JsonObject.fromMap(Map("events" -> EventSerialization.toJson(input.events), "shapeId" -> Json.fromString(input.shapeId))).asJson
  }

  override def deserializeOutput(json: Json): ShapeNameTask.Output = json.as[ShapeNameTask.Output].right.get

  override def summary(input: ShapeNameTask.Input, result: ShapeNameTask.Output): String = {
    result.flatStringName
  }

  override def transform(input: ShapeNameTask.Input): ShapeNameTask.Output = {
    implicit val ids = OpticIds.newDeterministicIdGenerator
    val eventStore = RfcServiceJSFacade.makeEventStore()
    val rfcId = "testRfcId"
    eventStore.append(rfcId, input.events)
    val rfcService = new RfcService(eventStore)
    val state = rfcService.currentState(rfcId)

    val namer = new ShapeNameRenderer(state)

    val shapeName = namer.nameForShapeId(input.shapeId).get
    ShapeNameTask.Output(shapeName.map(_.text).mkString(" "), shapeName.map(_.toString()).toVector)
  }
}
