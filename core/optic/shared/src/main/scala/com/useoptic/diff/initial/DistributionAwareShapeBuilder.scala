package com.useoptic.diff.initial

import com.useoptic.contexts.rfc.{RfcCommandContext, RfcService, RfcServiceJSFacade, RfcState}
import com.useoptic.contexts.shapes.Commands._
import com.useoptic.contexts.shapes.ShapesHelper._
import com.useoptic.diff.initial.DistributionAwareShapeBuilder.{buildCommandsFor, toShapes}
import com.useoptic.diff.shapes.JsonTrailPathComponent.{JsonArrayItem, JsonObjectKey}
import com.useoptic.diff.shapes._
import com.useoptic.diff.shapes.resolvers.JsonLikeResolvers
import com.useoptic.diff.{ImmutableCommandStream, MutableCommandStream}
import com.useoptic.dsa.{OpticDomainIds, SequentialIdGenerator}
import com.useoptic.types.capture.JsonLike
import com.useoptic.ux.{ColoredName, ShapeNameRenderer}

import scala.scalajs.js.annotation.{JSExport, JSExportAll}
import scala.util.Random

object DistributionAwareShapeBuilder {

  def streaming(implicit ids: OpticDomainIds, shapeBuildingStrategy: ShapeBuildingStrategy) = new StreamingShapeBuilder()

  def toCommands(bodies: Vector[JsonLike])(implicit ids: OpticDomainIds, shapeBuildingStrategy: ShapeBuildingStrategy): (ShapeId, ImmutableCommandStream) = {

    val aggregator = aggregateTrailsAndValues(bodies)

    implicit val commands = new MutableCommandStream

    val rootShape = toShapes(aggregator)
    buildCommandsFor(rootShape, None)

    (rootShape.id, commands.toImmutable)
  }

  def toCommandsWithName(bodies: Vector[JsonLike])(implicit ids: OpticDomainIds, shapeBuildingStrategy: ShapeBuildingStrategy): (ShapeId, ImmutableCommandStream, String) = {
    val (rootShape, commands) = toCommands(bodies)

    val eventStore = RfcServiceJSFacade.makeEventStore()
    val simulatedId = "simulated-id"
    val rfcService = new RfcService(eventStore)
    rfcService.handleCommandSequence(simulatedId, commands.flatten, RfcCommandContext("", "", ""))

    val currentState = rfcService.currentState(simulatedId)

    val namer = new ShapeNameRenderer(currentState)
    val flatName = namer.nameForShapeId(rootShape).get.map(_.text).mkString(" ").trim()

    (rootShape, commands, flatName)
  }

  def buildCommandsFor(shape: ShapesToMake, parent: Option[ShapesToMake])(implicit commands: MutableCommandStream, ids: OpticDomainIds): Unit = {

    def inField = parent.isDefined && parent.get.isInstanceOf[FieldWithShape]
    def inShape = !inField

    shape match {
      case s: ObjectWithFields => {
        commands.appendInit(AddShape(s.id, ObjectKind.baseShapeId, ""))
        s.fields.foreach(field => buildCommandsFor(field, Some(s)))
      }
      case s: OptionalShape => {
        buildCommandsFor(s.shape, Some(s))
        commands.appendDescribe(SetParameterShape(
          ProviderInShape(s.id, ShapeProvider(s.shape.id), OptionalKind.innerParam)
        ))
        commands.appendInit(AddShape(s.id, OptionalKind.baseShapeId, ""))
      }
      case s: NullableShape => {
        buildCommandsFor(s.shape, Some(s))
        commands.appendDescribe(SetParameterShape(
          ProviderInShape(s.id, ShapeProvider(s.shape.id), NullableKind.innerParam)
        ))
        commands.appendInit(AddShape(s.id, NullableKind.baseShapeId, ""))
      }
      case s: FieldWithShape => {
        assert(parent.isDefined && parent.get.isInstanceOf[ObjectWithFields], "Fields must have a parent")
        buildCommandsFor(s.shape, Some(s))
        commands.appendInit(AddField(s.id, parent.get.id, s.key, FieldShapeFromShape(s.id, s.shape.id)))
      }
      case s: PrimitiveKind => {
        commands.appendInit(AddShape(s.id, s.baseShape.baseShapeId, ""))
      }
      case s: OneOfShape => {
        s.branches.foreach(branch => {
          buildCommandsFor(branch, Some(s))
          val paramId = ids.newShapeParameterId
          commands.appendDescribe(AddShapeParameter(paramId, s.id, ""))
          commands.appendDescribe(SetParameterShape(
            ProviderInShape(s.id, ShapeProvider(branch.id), paramId)
          ))
        })

        commands.appendInit(AddShape(s.id, OneOfKind.baseShapeId, ""))
      }
      case s: ListOfShape => {
        buildCommandsFor(s.shape, Some(s))
        commands.appendInit(AddShape(s.id, ListKind.baseShapeId, ""))
        commands.appendDescribe(SetParameterShape(
          ProviderInShape(s.id, ShapeProvider(s.shape.id), ListKind.innerParam)
        ))
      }
      case s: Unknown => {
        commands.appendInit(AddShape(s.id, UnknownKind.baseShapeId, ""))
      }
    }
  }

  def aggregateTrailsAndValues(bodies: Vector[JsonLike])(implicit ids: OpticDomainIds, shapeBuildingStrategy: ShapeBuildingStrategy): TrailValueMap = {

    val aggregator = new TrailValueMap(shapeBuildingStrategy)

    val visitor = new ShapeBuilderVisitor(aggregator)

    val jsonLikeTraverser = new JsonLikeTraverser(RfcState.empty, visitor)

    bodies.foreach(body => jsonLikeTraverser.traverse(Some(body), JsonTrail(Seq.empty)))

    aggregator
  }

  def toShapes(trailValues: TrailValueMap): ShapesToMake = trailValues.getRoot.toShape
}

class StreamingShapeBuilder()(implicit ids: OpticDomainIds, shapeBuildingStrategy: ShapeBuildingStrategy) {

  private val aggregator = new TrailValueMap(shapeBuildingStrategy)
  private val visitor = new ShapeBuilderVisitor(aggregator)
  private val jsonLikeTraverser = new JsonLikeTraverser(RfcState.empty, visitor)

  def process(jsonLike: JsonLike) = jsonLikeTraverser.traverse(Some(jsonLike), JsonTrail(Seq.empty))

  @JSExport
  def toCommands: (String, ImmutableCommandStream) = {
    implicit val commands = new MutableCommandStream
    val rootShape = toShapes(aggregator)
    buildCommandsFor(rootShape, None)
    (rootShape.id, commands.toImmutable)
  }
  def toCommandsOptional: Option[(String, ImmutableCommandStream)] = {
    implicit val commands = new MutableCommandStream
    if (aggregator.hasRoot) {
      val rootShape = toShapes(aggregator)
      buildCommandsFor(rootShape, None)
      Some(rootShape.id, commands.toImmutable)
    } else None
  }
}

class FocusedStreamingShapeBuilder(trail: JsonTrail)(implicit ids: OpticDomainIds, shapeBuildingStrategy: ShapeBuildingStrategy) {

  private val aggregator = new TrailValueMap(shapeBuildingStrategy)
  val allAffordanceVisitor = new ShapeBuilderVisitor(aggregator)
  private var interactionCounter = AffordanceInteractionPointers()

  def process(jsonLike: JsonLike, interactionPointer: String) = {
    val normalizedTrailsVisitor = new DenormalizedTrailCollector(trail)
    val jsonLikeTraverser = new FocusedJsonLikeTraverser(trail, Set(allAffordanceVisitor, normalizedTrailsVisitor))
    jsonLikeTraverser.traverse(Some(jsonLike), JsonTrail(Seq.empty))

    val valueAtTrail = JsonLikeResolvers.tryResolveJsonTrail(trail, Some(jsonLike))
    interactionCounter = interactionCounter.handleJsonLike(valueAtTrail, normalizedTrailsVisitor, interactionPointer)
  }

  def serialize = ValueAffordanceSerializationWithCounter(aggregator.serialize, interactionCounter)

}

//// Shapes to Make
sealed trait ShapesToMake {
  def id: String
  def trail: JsonTrail
}

case class OptionalShape(shape: ShapesToMake, trail: JsonTrail, id: String) extends ShapesToMake
case class NullableShape(shape: ShapesToMake, trail: JsonTrail, id: String) extends ShapesToMake
case class OneOfShape(branches: Seq[ShapesToMake], trail: JsonTrail, id: String) extends ShapesToMake
case class ObjectWithFields(fields: Seq[FieldWithShape], trail: JsonTrail, id: String) extends ShapesToMake
case class ListOfShape(shape: ShapesToMake, trail: JsonTrail, id: String) extends ShapesToMake
case class FieldWithShape(key: String, shape: ShapesToMake, trail: JsonTrail, id: String) extends ShapesToMake
case class PrimitiveKind(baseShape: CoreShapeKind, trail: JsonTrail, id: String) extends ShapesToMake
case class Unknown(trail: JsonTrail, id: String) extends ShapesToMake

/// Strategy
case class ShapeBuildingStrategy(learnFromFirstOccurrenceOnly: Boolean)
object ShapeBuildingStrategy {
  val learnASingleInteraction = ShapeBuildingStrategy(true)
  val inferPolymorphism = ShapeBuildingStrategy(false)
}


class TrailValueMap(strategy: ShapeBuildingStrategy)(implicit ids: OpticDomainIds) {

  class ValueAffordanceMap(var trail: JsonTrail) {
    var wasString: Boolean = false
    var wasNumber: Boolean = false
    var wasBoolean: Boolean = false
    var wasNull: Boolean = false
    var wasArray: Boolean = false
    var wasObject: Boolean = false

    var fieldSet: Set[Set[String]] = Set.empty

    def serialize: ValueAffordanceSerialization = ValueAffordanceSerialization(
      trail,
      wasString, wasNumber, wasBoolean, wasNull, wasArray, wasObject, fieldSet
    )

    def touchObject(fields: Set[String]) = {
      wasObject = true
      fieldSet = fieldSet + fields
    }

    def isUnknown =
      !wasString &&
        !wasNumber &&
        !wasBoolean &&
        !wasNull &&
        !wasArray &&
        !wasObject

    def toShape: ShapesToMake = {
      val kindsSet: Set[Option[ShapesToMake]] = Set(
        if (wasString) Some(PrimitiveKind(StringKind, trail, ids.newShapeId)) else None,
        if (wasNumber) Some(PrimitiveKind(NumberKind, trail, ids.newShapeId)) else None,
        if (wasBoolean) Some(PrimitiveKind(BooleanKind, trail, ids.newShapeId)) else None,
        if (wasArray) Some({
          val itemTrail = trail.withChild(JsonArrayItem(0))
          val inner = _internal.getOrElseUpdate(itemTrail, new ValueAffordanceMap(itemTrail))
          inner.toShape
          ListOfShape(inner.toShape, trail, ids.newShapeId)
        }) else None,
        if (wasObject) Some({
          val unionOfKeys = fieldSet.flatten

          val optionalKeys = unionOfKeys.collect {
            case key if !fieldSet.forall(i => i.contains(key)) => key
          }

          val fields = unionOfKeys.toVector.sorted.map(key => {
            val fieldTrail = trail.withChild(JsonObjectKey(key))
            val inner = _internal.getOrElseUpdate(fieldTrail, new ValueAffordanceMap(fieldTrail))

            val isOptional = optionalKeys.contains(key)
            val innerShape = inner.toShape

            val fieldShape = if (isOptional) {
              OptionalShape(innerShape, fieldTrail, ids.newShapeId)
            } else {
              innerShape
            }

            FieldWithShape(key, fieldShape, fieldTrail, ids.newFieldId)
          })

          ObjectWithFields(fields, trail, ids.newShapeId)
        }) else None
      )

      val kinds = kindsSet.flatten

      val finalShape: ShapesToMake = if (kinds.size == 1) {
        kinds.head
      } else if (kinds.size > 1) {
        OneOfShape(kinds.toSeq.sortWith {
          case (_: PrimitiveKind, _) => true
          case (a: PrimitiveKind, b: PrimitiveKind) => a.baseShape.baseShapeId > b.baseShape.baseShapeId
          case _ => false
        }, trail, ids.newShapeId)
      } else {
        Unknown(trail, ids.newShapeId)
      }

      if (wasNull) {
        NullableShape(finalShape, trail, ids.newShapeId)
      } else {
        finalShape
      }
    }

  }

  private val _internal = scala.collection.mutable.Map[JsonTrail, ValueAffordanceMap]()

  def toMap = _internal.toMap
  def serialize: Vector[ValueAffordanceSerialization] = _internal.values.map(_.serialize).toVector
  def deserialize(vector: Vector[ValueAffordanceSerialization]): Unit = {
    vector.foreach{
      case item: ValueAffordanceSerialization => {
        val affordances = new ValueAffordanceMap(item.trail)
        affordances.wasString = item.wasString
        affordances.wasNumber = item.wasNumber
        affordances.wasNull = item.wasNull
        affordances.wasBoolean = item.wasBoolean
        affordances.wasArray = item.wasArray
        affordances.wasObject = item.wasObject
        affordances.fieldSet = item.fieldSet

        _internal.put(
          item.trail,
          affordances
        )
    }}
  }

  def putValue(trail: JsonTrail, value: JsonLike): Unit = {

    if (_internal.get(trail).isDefined && strategy.learnFromFirstOccurrenceOnly) {
      return
    } else {

      val affordanceMap = _internal.getOrElseUpdate(trail, new ValueAffordanceMap(trail))

      if (value.isString) {
        affordanceMap.wasString = true
      }
      if (value.isNumber) {
        affordanceMap.wasNumber = true
      }
      if (value.isBoolean) {
        affordanceMap.wasBoolean = true
      }
      if (value.isNull) {
        affordanceMap.wasNull = true
      }
      if (value.isArray) {
        affordanceMap.wasArray = true
      }
      if (value.isObject) {
        affordanceMap.touchObject(value.fields.keySet)
      }
    }
  }

  def getRoot: ValueAffordanceMap = _internal(JsonTrail(Seq.empty))
  def getJsonTrail(trail: JsonTrail): Option[ValueAffordanceMap] = _internal.get(trail)
  def hasRoot: Boolean = _internal.contains(JsonTrail(Seq.empty))

  def hasTrail(trail: JsonTrail) = _internal.contains(trail)

}
class ShapeBuilderVisitor(aggregator: TrailValueMap) extends JsonLikeVisitors {

  def normalizeTrail(jsonTrail: JsonTrail): JsonTrail = {
    JsonTrail(jsonTrail.path.map {
      case JsonArrayItem(_) => JsonArrayItem(0)
      case a => a
    })
  }

  override val objectVisitor: ObjectVisitor = new ObjectVisitor {
    override def visit(value: JsonLike, bodyTrail: JsonTrail): Unit = aggregator.putValue(normalizeTrail(bodyTrail), value)
  }
  override val arrayVisitor: ArrayVisitor =new ArrayVisitor {
    override def visit(value: JsonLike, bodyTrail: JsonTrail): Unit = aggregator.putValue(normalizeTrail(bodyTrail), value)
  }
  override val primitiveVisitor: PrimitiveVisitor = new PrimitiveVisitor {
    override def visit(value: JsonLike, bodyTrail: JsonTrail): Unit = aggregator.putValue(normalizeTrail(bodyTrail), value)
  }
}

class DenormalizedTrailCollector(jsonTrail: JsonTrail)(implicit id: OpticDomainIds) extends JsonLikeVisitors {

  private val aggregator = new TrailValueMap(ShapeBuildingStrategy.inferPolymorphism)

  def handle(value: JsonLike, bodyTrail: JsonTrail) = {
    if (jsonTrail.compareLoose(bodyTrail)) {
      aggregator.putValue(bodyTrail, value)
    }
  }

  val lastFieldOption = jsonTrail.path.lastOption.collect {
    case a: JsonObjectKey => a
  }

  private lazy val asMapLazy = aggregator.toMap

  private val _missing = scala.collection.mutable.ListBuffer[JsonTrail]()

  def wasStringTrails: Vector[JsonTrail] = asMapLazy.collect {case (trail, affordances) if affordances.wasString => trail }.toVector.distinct
  def wasNumberTrails: Vector[JsonTrail] = asMapLazy.collect {case (trail, affordances) if affordances.wasNumber => trail }.toVector.distinct
  def wasBooleanTrails: Vector[JsonTrail] = asMapLazy.collect {case (trail, affordances) if affordances.wasBoolean => trail }.toVector.distinct
  def wasNullTrails: Vector[JsonTrail] = asMapLazy.collect {case (trail, affordances) if affordances.wasNull => trail }.toVector.distinct
  def wasArrayTrails: Vector[JsonTrail] = asMapLazy.collect {case (trail, affordances) if affordances.wasArray => trail }.toVector.distinct
  def wasObjectTrails: Vector[JsonTrail] = asMapLazy.collect {case (trail, affordances) if affordances.wasObject => trail }.toVector.distinct
  def wasMissingTrails: Vector[JsonTrail] = _missing.toVector

  override val objectVisitor: ObjectVisitor = new ObjectVisitor {
    override def visit(value: JsonLike, bodyTrail: JsonTrail): Unit = {

      if (lastFieldOption.isDefined) { // it is a field
        val expectedTrail = bodyTrail.withChild(lastFieldOption.get)
        if (expectedTrail.compareLoose(jsonTrail)) {  // if it's here, it would match the focus
          val missing = value.asJson.asObject.exists(obj => !obj.contains(lastFieldOption.get.key))
          if (missing) {
            _missing.append(expectedTrail)
          }
        }
      }
      handle(value, bodyTrail)
    }
  }
  override val arrayVisitor: ArrayVisitor =new ArrayVisitor {
    override def visit(value: JsonLike, bodyTrail: JsonTrail): Unit = handle(value, bodyTrail)
  }
  override val primitiveVisitor: PrimitiveVisitor = new PrimitiveVisitor {
    override def visit(value: JsonLike, bodyTrail: JsonTrail): Unit = handle(value, bodyTrail)
  }
}
