package com.useoptic.diff.shapes

import com.useoptic.contexts.rfc.RfcState
import com.useoptic.diff.shapes.JsonTrailPathComponent.{JsonArrayItem, JsonObjectKey}
import com.useoptic.types.capture.JsonLike

abstract class GenericJsonVisitor {
  def visit(value: JsonLike, bodyTrail: JsonTrail)
}

class JsonLikeTraverser(spec: RfcState, visitors: JsonLikeVisitors) {
  def traverse(body: Option[JsonLike], bodyTrail: JsonTrail): Unit = {
    if (body.isDefined) {
      val bodyJson = body.get
      if (bodyJson.isArray) {
        visitors.arrayVisitor.visit(bodyJson, bodyTrail)
        bodyJson.distinctItemsByIndex.foreach{ case (index, item) => {
          val itemTrail = bodyTrail.withChild(JsonArrayItem(index))
          traverse(Some(item), itemTrail)
        }}
      } else if (bodyJson.isObject) {
        visitors.objectVisitor.visit(bodyJson, bodyTrail)
        bodyJson.fields.foreach{ case (key, value) => {
          val fieldTrail = bodyTrail.withChild(JsonObjectKey(key))
          traverse(Some(value), fieldTrail)
        }}
      } else {
        visitors.primitiveVisitor.visit(bodyJson, bodyTrail)
      }
    }
  }
}


class FocusedJsonLikeTraverser(baseTrail: JsonTrail, visitors: JsonLikeVisitors) {
  def shouldVisit(trail: JsonTrail): Boolean = {
    val minLength = Set(trail.path.size, baseTrail.path.size).min
    JsonTrail(baseTrail.path.take(minLength)).compareLoose(
      JsonTrail(trail.path.take(minLength))
    )
  }
  def traverse(body: Option[JsonLike], bodyTrail: JsonTrail): Unit = {
    val s = shouldVisit(bodyTrail)
    if (body.isDefined && shouldVisit(bodyTrail)) {
      val bodyJson = body.get
      if (bodyJson.isArray) {
        visitors.arrayVisitor.visit(bodyJson, bodyTrail)
        bodyJson.distinctItemsByIndex.foreach{ case (index, item) => {
          val itemTrail = bodyTrail.withChild(JsonArrayItem(0)) // normalized by default
          traverse(Some(item), itemTrail)
        }}
      } else if (bodyJson.isObject) {
        visitors.objectVisitor.visit(bodyJson, bodyTrail)
        bodyJson.fields.foreach{ case (key, value) => {
          val fieldTrail = bodyTrail.withChild(JsonObjectKey(key))
          traverse(Some(value), fieldTrail)
        }}
      } else {
        visitors.primitiveVisitor.visit(bodyJson, bodyTrail)
      }
    }
  }
}

class JsonLikeTraverserWithSpecStubs(spec: RfcState, visitors: JsonLikeAndSpecVisitors) {

  def traverse(body: Option[JsonLike], bodyTrail: JsonTrail): Unit = {
    if (body.isDefined) {
      val bodyJson = body.get
      if (bodyJson.isArray) {
        visitors.arrayVisitor.visit(bodyJson, bodyTrail, ShapeTrail("", Seq.empty), Seq.empty, _ => Unit)
        bodyJson.items.zipWithIndex.foreach{ case (item, index) => {
          val itemTrail = bodyTrail.withChild(JsonArrayItem(index))
          traverse(Some(item), itemTrail)
        }}
      } else if (bodyJson.isObject) {
        visitors.objectVisitor.visit(bodyJson, bodyTrail, ShapeTrail("", Seq.empty), Seq.empty, (_, __) => Unit)
        bodyJson.fields.foreach{ case (key, value) => {
          val fieldTrail = bodyTrail.withChild(JsonObjectKey(key))
          traverse(Some(value), fieldTrail)
        }}
      } else {
        visitors.primitiveVisitor.visit(bodyJson, bodyTrail, ShapeTrail("", Seq.empty), Seq.empty)
      }
    }
  }
}
