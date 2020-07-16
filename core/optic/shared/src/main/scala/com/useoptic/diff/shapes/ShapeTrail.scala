package com.useoptic.diff.shapes

import com.useoptic.contexts.shapes.Commands.{FieldId, ShapeId, ShapeParameterId}

sealed trait ShapeTrailPathComponent {
  def namedShape: ShapeId
}

case class ObjectTrail(shapeId: ShapeId) extends ShapeTrailPathComponent {def namedShape = shapeId}

case class ObjectFieldTrail(fieldId: FieldId, fieldShapeId: ShapeId) extends ShapeTrailPathComponent {def namedShape = fieldShapeId}

case class ListTrail(shapeId: ShapeId) extends ShapeTrailPathComponent {def namedShape = shapeId}

case class ListItemTrail(listShapeId: ShapeId, itemShapeId: ShapeId) extends ShapeTrailPathComponent {def namedShape = itemShapeId}

case class OneOfTrail(shapeId: ShapeId) extends ShapeTrailPathComponent  {def namedShape = shapeId}

case class OneOfItemTrail(oneOfId: ShapeId, parameterId: ShapeParameterId, itemShapeId: ShapeId) extends ShapeTrailPathComponent {def namedShape = itemShapeId}

case class OptionalTrail(shapeId: ShapeId) extends ShapeTrailPathComponent {def namedShape = shapeId}

case class OptionalItemTrail(shapeId: ShapeId, innerShapeId: ShapeId) extends ShapeTrailPathComponent {def namedShape = innerShapeId}

case class NullableTrail(shapeId: ShapeId) extends ShapeTrailPathComponent {def namedShape = shapeId}

case class NullableItemTrail(shapeId: ShapeId, innerShapeId: ShapeId) extends ShapeTrailPathComponent {def namedShape = innerShapeId}

case class UnknownTrail() extends ShapeTrailPathComponent {def namedShape = ""}

case class ShapeTrail(rootShapeId: ShapeId, path: Seq[ShapeTrailPathComponent]) {

  def withoutParent(): ShapeTrail = {
    this.copy(path = this.path.tail)
  }

  def withChild(pc: ShapeTrailPathComponent) = {
    this.copy(path = path :+ pc)
  }

  def withChildren(pc: ShapeTrailPathComponent*) = {
    this.copy(path = path ++ pc)
  }

  def lastField(): Option[FieldId] = {
    path.lastOption match {
      case Some(pathComponent) => {
        pathComponent match {
          case pc: ObjectFieldTrail => Some(pc.fieldId)
          case _ => None
        }
      }
      case None => None
    }
  }

  def lastObject(): Option[ShapeId] = {
    path.lastOption match {
      case Some(pathComponent) => {
        pathComponent match {
          case ObjectTrail(shapeId) => Some(shapeId)
          case _ => None
        }
      }
      case None => Some(rootShapeId)
    }
  }

  def lastListItem(): Option[ListItemTrail] = {
    path.lastOption match {
      case Some(pathComponent) => {
        pathComponent match {
          case a: ListItemTrail => Some(a)
          case _ => None
        }
      }
      case None => None
    }
  }

  def parentTrail(): Option[ShapeTrail] = {
    path.lastOption.flatMap {
      case x => Some(this.copy(path = this.path.init))
    }
  }
}
