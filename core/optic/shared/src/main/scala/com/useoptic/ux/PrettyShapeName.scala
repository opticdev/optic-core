package com.useoptic.ux

import com.useoptic.contexts.rfc.RfcState
import com.useoptic.contexts.shapes.Commands.{FieldId, ShapeId}
import com.useoptic.contexts.shapes.{ShapeEntity, ShapesHelper}
import com.useoptic.contexts.shapes.ShapesHelper.{AnyKind, BooleanKind, ListKind, NullableKind, NumberKind, ObjectKind, OptionalKind, StringKind, UnknownKind}
import com.useoptic.diff.DiffResult
import com.useoptic.diff.shapes.{GenericWrapperVisitor, ListItemTrail, ListShapeVisitor, ObjectFieldTrail, ObjectShapeVisitor, OneOfVisitor, PrimitiveShapeVisitor, ShapeDiffResult, ShapeTrail, ShapeTraverser, ShapeVisitors}
import com.useoptic.diff.shapes.resolvers.{DefaultShapesResolvers, ShapesResolvers}
import com.useoptic.diff.shapes.resolvers.ShapesResolvers.ResolvedTrail
import com.useoptic.types.capture.JsonLike
import com.useoptic.ux.ShapeRenderInterfaces.{SpecArray, SpecField, SpecObject, SpecOneOf, SpecPrimitive, SpecRenderVisitorHelper, SpecShape, WrappedType}

object PrettyShapeName {

  def object_(specObjectId: ShapeId, fieldCount: Int) = {
    RenderName(Seq(
      NameComponent(ObjectKind.name, ObjectKind.color, link = Some(specObjectId)),
      NameComponent(s"(${fieldCount} ${if (fieldCount == 1) "field" else "fields"})", "modifier")
    ))
  }

  def nullable(innerShapeId: Option[ShapeId]): RenderName =
    RenderName(Seq(NameComponent("", "modifier", " (nullable)", innerShapeId)))

  def optional(innerShapeId: Option[ShapeId]): RenderName =
    RenderName(Seq(NameComponent("", "modifier", " (optional)", innerShapeId)))

  def oneOf(branches: Seq[ShapeId]): RenderName = {
    val nameComponents = branches.map(branch => {
      NameComponent(if (branches.lastOption.contains(branch) && branches.size > 1) "or " else "", "modifier", endText = if (branches.lastOption.contains(branch)) "" else ", ", inner = Some(branch), link = Some(branch))
    })
    RenderName(nameComponents)
  }

  def list(innerItem: Option[ShapeId], baseItem: Option[ShapeId]): RenderName =
    RenderName(Seq(NameComponent("List of ", ListKind.color, inner = innerItem, link = baseItem)))

  def any: RenderName = RenderName(Seq(NameComponent(AnyKind.name, AnyKind.color)))
  def string: RenderName = RenderName(Seq(NameComponent(StringKind.name, StringKind.color)))
  def number: RenderName = RenderName(Seq(NameComponent(NumberKind.name, NumberKind.color)))
  def boolean: RenderName = RenderName(Seq(NameComponent(BooleanKind.name, BooleanKind.color)))
  def unknown: RenderName = RenderName(Seq(NameComponent(UnknownKind.name, UnknownKind.color)))
}

class ShapeNameRenderer(resolvers: ShapesResolvers, spec: RfcState) {

  def this(spec:RfcState) = this(new DefaultShapesResolvers(spec), spec)

  def nameForShapeId(shapeId: ShapeId): Option[Seq[ColoredName]] = {

    val shapeNameVisitor = new ShapeNameRenderVisitor(resolvers, spec)
    val specTraverser = new ShapeTraverser(resolvers, spec, shapeNameVisitor)
    specTraverser.traverse(shapeId, ShapeTrail(shapeId, Seq()))

    val rootName = shapeNameVisitor.names.get(shapeId)
    val flat = rootName.map(_.asColoredStringFromNames(shapeNameVisitor.names))
    flat
  }

}


class ShapeNameRenderVisitor(resolvers: ShapesResolvers, spec: RfcState) extends ShapeVisitors {
  private val _names = scala.collection.mutable.Map[String, RenderName]()

  def names: Map[String, RenderName] = _names.toMap

  def pushName(shapeId: String, renderName: RenderName): Unit = {
    _names.put(shapeId, renderName)
  }

  override val objectVisitor: ObjectShapeVisitor = new ObjectShapeVisitor {
    override def begin(objectResolved: ResolvedTrail, shapeTrail: ShapeTrail, exampleJson: Option[JsonLike]): Unit = {

      val expectedFieldsSize = objectResolved.shapeEntity.descriptor.fieldOrdering.size
      val id = objectResolved.shapeEntity.shapeId

      pushName(
        id,
        PrettyShapeName.object_(id, expectedFieldsSize)
      )

    }

    override def visit(key: String, fieldId: FieldId, fieldShapeTrail: ResolvedTrail, fieldTrail: ShapeTrail): Unit = {

    }

    override def end(): Unit = ???

  }
  override val listVisitor: ListShapeVisitor = new ListShapeVisitor {

    override def begin(shapeTrail: ShapeTrail, listShape: ShapeEntity, itemShape: ShapeEntity): Unit = {

      val baseItem = resolvers.resolveToBaseShape(itemShape.shapeId)

      pushName(
        listShape.shapeId,
        PrettyShapeName.list(Some(baseItem.shapeId), Some(baseItem.shapeId)),
      )
    }

    override def end(): Unit = ???


    override def visit(): Unit = ???
  }
  override val primitiveVisitor: PrimitiveShapeVisitor = new PrimitiveShapeVisitor {
    override def visit(objectResolved: ResolvedTrail, shapeTrail: ShapeTrail): Unit = {

      val name = {
        objectResolved.coreShapeKind match {
          case ShapesHelper.AnyKind => PrettyShapeName.any
          case ShapesHelper.StringKind => PrettyShapeName.string
          case ShapesHelper.NumberKind => PrettyShapeName.number
          case ShapesHelper.BooleanKind => PrettyShapeName.boolean
          case ShapesHelper.UnknownKind => PrettyShapeName.unknown
          case _ => RenderName(Seq.empty)
        }
      }

      pushName(objectResolved.shapeEntity.shapeId, name)
    }
  }
  override val oneOfVisitor: OneOfVisitor = new OneOfVisitor {
    override def begin(shapeTrail: ShapeTrail, oneOfShape: ShapeEntity, branches: Seq[ShapeId]): Unit = {
      pushName(
        oneOfShape.shapeId,
        PrettyShapeName.oneOf(branches),
      )
    }

    override def visit(shapeTrail: ShapeTrail, oneOfShape: ShapeEntity, branchShape: ShapeEntity): Unit = {
    }

    override def end(): Unit = ???
  }
  override val optionalVisitor: GenericWrapperVisitor = new GenericWrapperVisitor {
    override def begin(shapeTrail: ShapeTrail, shape: ShapeEntity, innerShape: Option[ShapeEntity]): Unit = {
      pushName(shape.shapeId,PrettyShapeName.optional(innerShape.map(_.shapeId)))
    }
  }
  override val nullableVisitor: GenericWrapperVisitor = new GenericWrapperVisitor {
    override def begin(shapeTrail: ShapeTrail, shape: ShapeEntity, innerShape: Option[ShapeEntity]): Unit = {
      pushName(shape.shapeId,PrettyShapeName.nullable(innerShape.map(_.shapeId)))
    }
  }
}
