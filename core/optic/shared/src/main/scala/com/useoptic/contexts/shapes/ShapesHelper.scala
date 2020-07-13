package com.useoptic.contexts.shapes

import com.useoptic.contexts.requests.Commands.ShapedRequestParameterShapeDescriptor
import com.useoptic.contexts.requests.Events._
import com.useoptic.contexts.rfc.Events.EventContext
import com.useoptic.contexts.shapes.Commands._
import com.useoptic.contexts.shapes.Events._
import com.useoptic.ddd.{AggregateId, EventSourcedRepository, InMemoryEventStore}
import com.useoptic.dsa.{OpticDomainIds, OpticIds, RandomAlphanumericIdGenerator}

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExport
@JSExportAll
object ShapesHelper {
  def appendDefaultStringTypeEvents(adder: RequestParameterAdded, eventContext: Option[EventContext])(implicit ids: OpticDomainIds): Vector[RequestsEvent] = {
    val shapeId = ids.newShapeId
    Vector(
      adder,
      ShapeAdded(shapeId, "$string", DynamicParameterList(Seq.empty), "", eventContext),
      RequestParameterShapeSet(adder.parameterId, ShapedRequestParameterShapeDescriptor(shapeId, isRemoved = false), eventContext)
    ).asInstanceOf[Vector[RequestsEvent]]
  }

  def appendDefaultStringTypeEvents(adder: PathParameterAdded)(implicit ids: OpticDomainIds): Vector[RequestsEvent] = {
    val shapeId = ids.newShapeId

    Vector(
      adder,
      ShapeAdded(shapeId, "$string", DynamicParameterList(Seq.empty), ""),
      PathParameterShapeSet(adder.pathId, ShapedRequestParameterShapeDescriptor(shapeId, isRemoved = false))
    ).asInstanceOf[Vector[RequestsEvent]]
  }

  sealed class CoreShapeKind(val baseShapeId: ShapeId, val name: String) {def color = s"${name}Color"}
  case object ObjectKind extends CoreShapeKind("$object", "Object")
  case object ListKind extends CoreShapeKind("$list", "List") {def innerParam: String = "$listItem"}
  case object MapKind extends CoreShapeKind("$map", "Map")
  case object OneOfKind extends CoreShapeKind("$oneOf", "One of")
  case object AnyKind extends CoreShapeKind("$any", "Any")
  case object StringKind extends CoreShapeKind("$string", "String")
  case object NumberKind extends CoreShapeKind("$number", "Number")
  case object BooleanKind extends CoreShapeKind("$boolean", "Boolean")
  case object IdentifierKind extends CoreShapeKind("$identifier", "Identifier") {def innerParam: String = "$identifierInner"}
  case object ReferenceKind extends CoreShapeKind("$reference", "Reference") {def innerParam: String = "$referenceInner"}
  case object NullableKind extends CoreShapeKind("$nullable", "Nullable") {def innerParam: String = "$nullableInner"}
  case object OptionalKind extends CoreShapeKind("$optional", "Optional") {def innerParam: String = "$optionalInner"}
  case object UnknownKind extends CoreShapeKind("$unknown", "Unknown")

  val allCoreShapes = Set(ObjectKind, ListKind, MapKind, OneOfKind, AnyKind, StringKind, NumberKind, BooleanKind, IdentifierKind, ReferenceKind, NullableKind, OptionalKind, UnknownKind)

  def toCoreShape(shapeEntity: ShapeEntity, shapesState: ShapesState): CoreShapeKind = {
    allCoreShapes.find(_.baseShapeId == shapeEntity.descriptor.baseShapeId) match {
      case Some(shape) => shape
      case None => {
        toCoreShape(shapesState.shapes(shapeEntity.descriptor.baseShapeId), shapesState)
      }
    }
  }

  def toCoreAndBaseShape(shapeEntity: ShapeEntity, shapesState: ShapesState): (ShapeId, CoreShapeKind) = {
    allCoreShapes.find(_.baseShapeId == shapeEntity.descriptor.baseShapeId) match {
      case Some(shape) => (shapeEntity.shapeId, shape)
      case None => {
        (shapeEntity.descriptor.baseShapeId, toCoreShape(shapesState.shapes(shapeEntity.descriptor.baseShapeId), shapesState))
      }
    }
  }
}

//STRICTLY FOR TESTING (because everything should go through the root (RfcService))
class ShapesService()(implicit ids: OpticDomainIds) {
  private val eventStore = new InMemoryEventStore[ShapesEvent]
  private val repository = new EventSourcedRepository[ShapesState, ShapesEvent](ShapesAggregate, eventStore)

  def handleCommand(id: AggregateId, command: ShapesCommand): Unit = {
    val state = repository.findById(id)
    val effects = ShapesAggregate.handleCommand(state)(ids)((ShapesCommandContext("a", "b", "c"), command))
    repository.save(id, effects.eventsToPersist)
  }

  def currentState(id: AggregateId): ShapesState = {
    repository.findById(id)
  }
}

