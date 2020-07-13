package com.useoptic.diff.shapes

import com.useoptic.types.capture.JsonLike

abstract class ObjectVisitor {
  def visit(value: JsonLike, bodyTrail: JsonTrail)
}

abstract class ArrayVisitor {
  def visit(value: JsonLike, bodyTrail: JsonTrail)
}

abstract class PrimitiveVisitor {
  def visit(value: JsonLike, bodyTrail: JsonTrail)
}

abstract class JsonLikeVisitors {
  val objectVisitor: ObjectVisitor
  val arrayVisitor: ArrayVisitor
  val primitiveVisitor: PrimitiveVisitor
}
