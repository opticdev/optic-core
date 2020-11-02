package com.useoptic

import com.useoptic.contexts.requests.Commands.{PathComponentId, RequestId, RequestParameterId, ResponseId}
import com.useoptic.contexts.shapes.Commands.{FieldId, ShapeId, ShapeParameterId}
import com.useoptic.dsa.OpticIds

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExport
@JSExportAll
class OpticIdsJs(random: Boolean = true, prefix: String = "incrementing-id") {
  private val ids = if (random) OpticIds.newRandomIdGenerator else OpticIds.newPrefixedDeterministicIdGenerator(prefix)

  def newShapeId(): ShapeId = ids.newShapeId
  def newPathId(): PathComponentId = ids.newPathId
  def newRequestId(): RequestId = ids.newRequestId
  def newResponseId(): ResponseId = ids.newResponseId
  def newShapeParameterId(): ShapeParameterId = ids.newShapeParameterId
  def newRequestParameterId(): RequestParameterId = ids.newRequestParameterId
  def newFieldId(): FieldId = ids.newFieldId
}


@JSExport
@JSExportAll
object OpticIdsJsHelper {
  def random = new OpticIdsJs(true)
  def deterministic = new OpticIdsJs(false)
}
