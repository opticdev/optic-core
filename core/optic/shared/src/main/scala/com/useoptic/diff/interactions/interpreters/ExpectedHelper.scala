package com.useoptic.diff.interactions.interpreters

import com.useoptic.contexts.rfc.RfcState
import com.useoptic.contexts.shapes.Commands.{FieldId, ShapeId}
import com.useoptic.contexts.shapes.ShapesHelper.{ListKind, ObjectKind, OptionalKind}
import com.useoptic.diff.shapes.{ListItemTrail, ListTrail, NullableItemTrail, NullableTrail, ObjectFieldTrail, ObjectTrail, OneOfItemTrail, OneOfTrail, OptionalItemTrail, OptionalTrail, ShapeTrail, UnknownTrail}
import com.useoptic.diff.shapes.resolvers.ShapesResolvers
import com.useoptic.ux.ShapeNameRenderer
import io.circe.Json

import scala.scalajs.js.annotation.{JSExport, JSExportAll}
import scala.util.Try


case class ExpectedHelper(allowedCoreShapes: Seq[String],
                          allowedCoreShapeKindsByShapeId: Map[String, String],
                          lastField: Option[FieldId],
                          lastFieldKey: Option[String],
                          lastFieldShapeId: Option[String],
                          fieldIsOptional: Option[Boolean], // defined if field, true, if optional
                          lastObject: Option[ShapeId],
                          lastList: Option[ShapeId],
                          lastListItem: Option[ShapeId],
                          lastOneOf: Option[OneOfTrail],
                          lastOneOfItem: Option[OneOfItemTrail],
                          lastUnknownTrail: Option[UnknownTrail],
                          lastNullable: Option[NullableTrail],
                          lastOptionalItemTrail: Option[OptionalItemTrail],
                          rootShapeId: Option[String],
                          shapeName: Option[String])

@JSExport
@JSExportAll
object ExpectedHelper {

  def expectedForDiffStrings(shapeTrailRaw: String, rfcState: RfcState): Json = {
    import io.circe._, io.circe.parser._
    import io.circe.generic.auto._
    import io.circe.syntax._
    println("pre-parsed")
    expectedForDiff(parseShapeTrail(shapeTrailRaw), rfcState).asJson
  }

  def expectedForDiff(normalizedShapeTrail: ShapeTrail, rfcState: RfcState): ExpectedHelper = {

    println("a")
    val resolver = ShapesResolvers.newResolver(rfcState)
    println("b")
    val choices = resolver.listTrailChoices(normalizedShapeTrail, Map.empty)
    println("c")
    val coreShapeKindsByShapeId = choices
        .map(i => i.coreShapeKind.baseShapeId.toString -> i.shapeId).toMap
    println("d")
    val coreShapeKinds = coreShapeKindsByShapeId.keys.toSeq
    println("e")
    val shapeTrail = normalizedShapeTrail
    println("f")
    val lastField = shapeTrail.lastField()
    println("g")
    val lastFieldKey = lastField.map(fieldId => resolver.getField(fieldId)).map(_.descriptor.name)
    println("h")
    val lastFieldShapeId = lastField.map(fieldId => resolver.getField(fieldId)).map(_.descriptor.shapeId)
    println("i")
    val lastObject = {
      choices.collectFirst{
          case c if c.coreShapeKind == ObjectKind => c.shapeId
      }
    }
    println("j")

    val lastListItemTrail = shapeTrail.lastListItem()
    println("k")
    val lastListItem = lastListItemTrail.map(_.itemShapeId)
    println("l")
    val lastList = lastListItemTrail.map(_.listShapeId)
    println("m")


    println("n")
    val fieldIsOptional: Option[Boolean] = {
      if (lastField.isDefined) {
        val coreShapeKind = resolver.resolveTrailToCoreShape(shapeTrail, Map.empty).coreShapeKind
        Some(coreShapeKind == OptionalKind)
      } else None
    }
    println("o")


    val lastOneOfItemTrail: Option[OneOfItemTrail] = shapeTrail.path.lastOption collect  { case a: OneOfItemTrail => a}
    val lastOneOfTrail: Option[OneOfTrail] = shapeTrail.path.lastOption collect  { case a: OneOfTrail => a}
    val lastUnknownTrail: Option[UnknownTrail] = shapeTrail.path.lastOption collect  { case a: UnknownTrail => a}
    val lastNullable: Option[NullableTrail] = shapeTrail.path.lastOption collect  { case a: NullableTrail => a}
    val lastOptionalItemTrail: Option[OptionalItemTrail] = shapeTrail.path.lastOption collect  { case a: OptionalItemTrail => a}

    println("p")

    val includeRootShapeInName =
      shapeTrail.path.lastOption.map(i => i.namedShape).filterNot(i => i == "").getOrElse(shapeTrail.rootShapeId)
    val shapeName = Try { // fix for pesky scala js issue
      val name = new ShapeNameRenderer(rfcState).nameForShapeId(includeRootShapeInName)
      name.map(i => i.map(n => n.text).mkString(" "))
    }.toOption.flatten

    println("q")
    val rootShapeId = if (shapeTrail.path.isEmpty) Some(shapeTrail.rootShapeId) else None
    println("r")
    ExpectedHelper(coreShapeKinds,
      coreShapeKindsByShapeId,
      lastField,
      lastFieldKey,
      lastFieldShapeId,
      fieldIsOptional,
      lastObject,
      lastList,
      lastListItem,
      lastOneOfTrail,
      lastOneOfItemTrail,
      lastUnknownTrail,
      lastNullable,
      lastOptionalItemTrail,
      rootShapeId,
      shapeName)
  }

  def parseShapeTrail(shapeTrail: String): ShapeTrail = {
    import io.circe._, io.circe.parser._
    import io.circe.generic.auto._
    import io.circe.syntax._
    parse(shapeTrail).right.get.as[ShapeTrail].right.get
  }
}
