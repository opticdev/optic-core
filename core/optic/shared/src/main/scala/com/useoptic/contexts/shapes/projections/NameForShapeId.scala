package com.useoptic.contexts.shapes.projections

import com.useoptic.contexts.shapes.Commands._
import com.useoptic.contexts.shapes.ShapesHelper._
import com.useoptic.contexts.shapes.{ShapesHelper, ShapesState}
import com.useoptic.diff.shapes.resolvers.ShapesResolvers

import scala.scalajs.js.annotation.{JSExport, JSExportAll}
import scala.util.Try

@JSExportAll
case class ColoredComponent(name: String, colorKey: String, shapeLink: Option[String] = None, primitiveId: Option[String] = None)

class NameForShapeId(resolvers: ShapesResolvers, shapesState: ShapesState) {
  val flatShapeQueries = new FlatShapeQueries(resolvers, this, shapesState)

  private val returnAny = Seq(ColoredComponent("Any", "primitive", primitiveId = Some(AnyKind.baseShapeId)))

  def getFlatShapeName(shapeId: ShapeId)(implicit fieldIdOption: Option[String] = None): String = {
    getShapeName(shapeId)(fieldIdOption).map(_.name).mkString(" ")
  }

  def getFieldIdShapeName(fieldId: String): Seq[ColoredComponent] = {
    val field = shapesState.fields(fieldId)
    val result = flatShapeQueries.forShapeId(field.descriptor.shapeDescriptor.asInstanceOf[FieldShapeFromShape].shapeId, Some(fieldId))(true)
    result.root.typeName
  }

  def getShapeName(shapeId: ShapeId, expand: Boolean = false)(implicit fieldIdOption: Option[String] = None, seenIds: Seq[ShapeId] = Seq()): Seq[ColoredComponent] = {
    //prevent infinite loop
    if (seenIds.contains(shapeId)) {
      return returnAny
    }

    val conceptName = shapesState.concepts.get(shapeId).map(_.descriptor.name)
    if (conceptName.isDefined && !expand) {
      return Seq(ColoredComponent(conceptName.get, "concept", Some(shapeId)))
    }

    val shape = shapesState.flattenedShape(shapeId)

    def resolveInner(paramId: String) = resolvers.resolveParameterToShape(shapeId, paramId, {
      if (fieldIdOption.isDefined) {
        shapesState.flattenedField(fieldIdOption.get).bindings
      } else {
        shapesState.flattenedShape(shapeId).bindings
      }
    })
      .map(i => (i.descriptor.baseShapeId, getShapeName(i.shapeId)(fieldIdOption = fieldIdOption, seenIds = seenIds :+ shapeId)))
      .getOrElse(("$any", returnAny))

    shape.coreShapeId match {
      case ListKind.baseShapeId => {
        val (innerId, listItemComponent) = resolveInner("$listItem")
        Seq(
          ColoredComponent("List", "primitive", primitiveId = Some(ListKind.baseShapeId)),
          ColoredComponent("of", "text", None, None),
        ) ++ listItemComponent
      }
      case MapKind.baseShapeId => {
        val (keyId, mapKeyComponent) = resolveInner("$mapKey")
        val (valueId, mapValueComponent) = resolveInner("$mapValue")

        Seq(ColoredComponent("Map", "primitive", primitiveId = Some(MapKind.baseShapeId)), ColoredComponent("from", "text", None)) ++
          mapKeyComponent ++ Seq(ColoredComponent("to", "text", None)) ++ mapValueComponent
      }
      case OneOfKind.baseShapeId => {
        val inners = shapesState.shapes(shapeId).descriptor.parameters match {
          case DynamicParameterList(shapeParameterIds) => shapeParameterIds
        }
        val innersResolved = inners.map(i => resolveInner(i))

        innersResolved.zipWithIndex.flatMap {
          case ((id, ty), index) if index == inners.length - 1 => ColoredComponent("or", "text", None) +: ty
          case ((id, ty), index) if index == 0 => ty
          case ((id, ty), index) => ColoredComponent(",", "text", None) +: ty
        }
      }
      case NullableKind.baseShapeId => {
        val (innerId, nullableInner) = resolveInner("$nullableInner")
        nullableInner ++ Seq(ColoredComponent("(nullable)", "modifier", primitiveId = Some(NullableKind.baseShapeId)))
      }
      case OptionalKind.baseShapeId => {
        val (innerId, optionalInner) = resolveInner(OptionalKind.innerParam)
        optionalInner ++ Seq(ColoredComponent("(optional)", "modifier", primitiveId = Some(OptionalKind.baseShapeId)))
      }
      case ReferenceKind.baseShapeId => {
        val (innerId, referenceInner) = resolveInner("$referenceInner")
        Seq(ColoredComponent("Reference to", "text", None)) ++ referenceInner
      }
      case IdentifierKind.baseShapeId => {
        val (innerId, identifierInner) = resolveInner("$identifierInner")
        Seq(ColoredComponent("Identifier as", "text", None)) ++ identifierInner
      }
      case ObjectKind.baseShapeId => {
        val baseObject = resolvers.resolveBaseObject(shapeId)

        val genericParams = Try(baseObject.descriptor.parameters.asInstanceOf
          [DynamicParameterList].shapeParameterIds).getOrElse(Seq.empty)

        val generics = genericParams.flatMap(param => {
          val (id, seq) = resolveInner(param)
          Seq(ColoredComponent(param.split(":").last + ":", "text", None)) ++ seq
        })

        if (baseObject.descriptor.name != "") {
          Seq(ColoredComponent(baseObject.descriptor.name, "concept", Some(shapeId))) ++ generics
        } else {
          Seq(ColoredComponent("Object", "primitive", Some(shapeId), primitiveId = Some(ObjectKind.baseShapeId))) ++ generics
        }
      }

      //fallback to standard case
      case baseShapeId if ShapesHelper.allCoreShapes.exists(_.baseShapeId == baseShapeId) =>
        val name = ShapesHelper.allCoreShapes.find(_.baseShapeId == baseShapeId).map(_.name).get
        Seq(ColoredComponent(name, "primitive", None, primitiveId = Some(baseShapeId)))
      case _ => Seq()
    }
  }
}
