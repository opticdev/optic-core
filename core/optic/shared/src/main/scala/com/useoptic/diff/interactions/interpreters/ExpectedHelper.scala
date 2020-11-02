package com.useoptic.diff.interactions.interpreters

import com.useoptic.contexts.requests.Resolvers
import com.useoptic.contexts.rfc.{RfcService, RfcState}
import com.useoptic.contexts.shapes.Commands.{FieldId, ShapeId}
import com.useoptic.contexts.shapes.ShapesHelper.OptionalKind
import com.useoptic.diff.initial.ShapeResolver
import com.useoptic.diff.interactions.{BodyUtilities, ContentTypeHelpers, InteractionDiffResult}
import com.useoptic.diff.shapes.{ListItemTrail, ListTrail, NullableItemTrail, NullableTrail, ObjectFieldTrail, ObjectTrail, OneOfItemTrail, OneOfTrail, OptionalItemTrail, OptionalTrail, UnknownTrail}
import com.useoptic.diff.shapes.resolvers.ShapesResolvers
import com.useoptic.ux.ShapeNameRenderer
import io.circe.Json

import scala.scalajs.js.annotation.{JSExport, JSExportAll}


case class ExpectedHelper(allowedCoreShapes: Seq[String],
                          lastField: Option[FieldId],
                          lastFieldKey: Option[String],
                          lastFieldShapeId: Option[String],
                          fieldIsOptional: Option[Boolean], // defined if field, true, if optional
                          lastObject: Option[ShapeId],
                          lastListItem: Option[ListItemTrail],
                          lastOneOf: Option[OneOfTrail],
                          lastOneOfItem: Option[OneOfItemTrail],
                          lastUnknownTrail: Option[UnknownTrail],
                          lastNullable: Option[NullableTrail],
                          lastOptionalItemTrail: Option[OptionalItemTrail],
                          shapeName: Option[String])

@JSExport
@JSExportAll
object ExpectedHelper {

  def expectedForDiffStrings(diffsRaw: String, rfcState: RfcState): Json = {
    import io.circe._, io.circe.parser._
    import io.circe.generic.auto._
    import io.circe.syntax._
    expectedForDiff(parseDiffs(diffsRaw), rfcState).asJson
  }

  def expectedForDiff(diffs: Seq[InteractionDiffResult], rfcState: RfcState): ExpectedHelper = {

    val resolver = ShapesResolvers.newResolver(rfcState)

    val coreShapeKinds = diffs.flatMap(i => {
      resolver.listTrailChoices(i.shapeDiffResultOption.get.shapeTrail, Map.empty)
    }).map(i => i.coreShapeKind).distinct.map(_.baseShapeId)

    val shapeTrail = diffs.head.shapeDiffResultOption.get.shapeTrail

    val lastField = shapeTrail.lastField()

    val lastFieldKey = lastField.map(fieldId => resolver.getField(fieldId)).map(_.descriptor.name)
    val lastFieldShapeId = lastField.map(fieldId => resolver.getField(fieldId)).map(_.descriptor.shapeId)

    val lastObject = shapeTrail.lastObject()
    val lastListItem = shapeTrail.lastListItem()


    val fieldIsOptional: Option[Boolean] = {
      if (lastField.isDefined) {
        val coreShapeKind = resolver.resolveTrailToCoreShape(shapeTrail, Map.empty).coreShapeKind
        Some(coreShapeKind == OptionalKind)
      } else None
    }


    val lastOneOfItemTrail: Option[OneOfItemTrail] = shapeTrail.path.lastOption collect  { case a: OneOfItemTrail => a}
    val lastOneOfTrail: Option[OneOfTrail] = shapeTrail.path.lastOption collect  { case a: OneOfTrail => a}
    val lastUnknownTrail: Option[UnknownTrail] = shapeTrail.path.lastOption collect  { case a: UnknownTrail => a}
    val lastNullable: Option[NullableTrail] = shapeTrail.path.lastOption collect  { case a: NullableTrail => a}
    val lastOptionalItemTrail: Option[OptionalItemTrail] = shapeTrail.path.lastOption collect  { case a: OptionalItemTrail => a}


    val shapeName = shapeTrail.path.lastOption.map(i => i.namedShape).filterNot(i => i == "").flatMap(i => {
      val name = new ShapeNameRenderer(ShapesResolvers.newResolver(rfcState), rfcState).nameForShapeId(i)
      name.map(i => i.map(n => n.text).mkString(" "))
    })

    ExpectedHelper(coreShapeKinds,
      lastField,
      lastFieldKey,
      lastFieldShapeId,
      fieldIsOptional,
      lastObject,
      lastListItem,
      lastOneOfTrail,
      lastOneOfItemTrail,
      lastUnknownTrail,
      lastNullable,
      lastOptionalItemTrail,
      shapeName)
  }

  def parseDiffs(diffsRaw: String): Seq[InteractionDiffResult] = {
    import io.circe._, io.circe.parser._
    import io.circe.generic.auto._
    import io.circe.syntax._
    parse(diffsRaw).right.get.as[Seq[InteractionDiffResult]].right.get
  }
}
