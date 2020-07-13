package com.useoptic.ux

import com.useoptic.contexts.rfc.Commands.RfcCommand
import com.useoptic.contexts.rfc.{Events, RfcCommandContext, RfcService, RfcState}
import com.useoptic.contexts.shapes.Commands.{FieldId, ShapeId}
import com.useoptic.contexts.shapes.ShapesHelper._
import com.useoptic.contexts.shapes.{ShapeEntity, ShapesHelper}
import com.useoptic.ddd.InMemoryEventStore
import com.useoptic.diff.DiffResult
import com.useoptic.diff.initial.{DistributionAwareShapeBuilder, ShapeBuildingStrategy}
import com.useoptic.diff.interactions.BodyUtilities
import com.useoptic.diff.shapes.JsonTrailPathComponent.{JsonArrayItem, JsonObjectKey}
import com.useoptic.diff.shapes.Stuff.{ArrayItemChoiceCallback, ObjectKeyChoiceCallback}
import com.useoptic.diff.shapes.resolvers.ShapesResolvers.{ChoiceOutput, ResolvedTrail}
import com.useoptic.diff.shapes.resolvers.{DefaultShapesResolvers, JsonLikeResolvers, ShapesResolvers}
import com.useoptic.diff.shapes.{JsonTrail, _}
import com.useoptic.dsa.OpticIds
import com.useoptic.logging.Logger
import com.useoptic.types.capture.{Body, JsonLike}
import com.useoptic.ux.ExampleRenderInterfaces._
import com.useoptic.ux.ShapeRenderInterfaces._

import scala.scalajs.js.annotation.{JSExport, JSExportAll}
import scala.util.Try

@JSExport
@JSExportAll
class DiffPreviewer(resolvers: ShapesResolvers, spec: RfcState) {

  //temp helper while migrating code
  def this(spec:RfcState) = this(new DefaultShapesResolvers(spec), spec)

  ///@todo make return optional
  def previewDiff(jsonLike: Option[JsonLike], shapeIdOption: Option[ShapeId], diffs: Set[ShapeDiffResult], allDiffs: Set[ShapeDiffResult]): Option[SideBySideRenderHelper] = shapeIdOption map { shapeId =>

    val relatedDiffs: Map[ShapeDiffResult, Set[JsonTrail]] = diffs.collect {
      case diff: ShapeDiffResult if diffs.contains(diff) => diff -> allDiffs.filter(_.shapeTrail == diff.shapeTrail).map(_.jsonTrail).toSet
    }.toMap

    println("xxxx SHOWING RELATED DIFFS at JSON trails "+ relatedDiffs.values.flatten.mkString("\n"))


    val shapeRenderVisitor = new ShapeRenderVisitor(resolvers, spec, diffs)
    //first traverse the example
    val exampleRenderVisitor = new ExampleRenderVisitorNew(resolvers, spec, diffs, relatedDiffs)
    val jsonLikeTraverser = new JsonLikeAndSpecTraverser(resolvers, spec, exampleRenderVisitor)
    jsonLikeTraverser.traverseRootShape(jsonLike, shapeId)

    val specTraverser = new ShapeTraverser(resolvers, spec, shapeRenderVisitor)
    specTraverser.traverse(shapeId, ShapeTrail(shapeId, Seq()))

    new SideBySideRenderHelper(
      exampleRenderVisitor.shapes,
      exampleRenderVisitor.fields,
      exampleRenderVisitor.items,
      shapeRenderVisitor.shapes,
      exampleRenderVisitor.rootShape.exampleId
    )
  }


  def previewShape(shapeIdOption: Option[ShapeId]): Option[ShapeOnlyRenderHelper] = shapeIdOption map { shapeId =>
    val shapeRenderVisitor = new ShapeRenderVisitor(resolvers, spec, Set.empty)
    val specTraverser = new ShapeTraverser(resolvers, spec, shapeRenderVisitor)
    specTraverser.traverse(shapeId, ShapeTrail(shapeId, Seq()))
    new ShapeOnlyRenderHelper(shapeRenderVisitor.shapes, shapeRenderVisitor.rootShape.specShapeId)
  }

  def previewBody(body: Body): Option[SideBySideRenderHelper] = {
    BodyUtilities.parseBody(body).map(body => {
      val exampleRenderVisitor = new ExampleRenderVisitorNew(resolvers, RfcState.empty, Set.empty, Map.empty)
      val jsonLikeTraverser = new JsonLikeTraverserWithSpecStubs(RfcState.empty, exampleRenderVisitor)
      jsonLikeTraverser.traverse(Some(body), JsonTrail(Seq.empty))

      new SideBySideRenderHelper(
        exampleRenderVisitor.shapes,
        exampleRenderVisitor.fields,
        exampleRenderVisitor.items,
        Map.empty,
        JsonTrail(Seq.empty).toString
      )
    })
  }

  def shapeOnlyFromShapeBuilder(bodies: Vector[JsonLike])(implicit shapeBuildingStrategy: ShapeBuildingStrategy): Option[(Vector[RfcCommand], ShapeOnlyRenderHelper)] = {

    if (bodies.isEmpty) {
      return None
    }


    val (shapeId, commands) = DistributionAwareShapeBuilder.toCommands(bodies)(ids = OpticIds.generator, shapeBuildingStrategy)
    val flattenedCommands = commands.flatten

    val simulatedId = "simulated"
    val commandContext: RfcCommandContext = RfcCommandContext("a", "b", "c")
    val service = new RfcService(new InMemoryEventStore[Events.RfcEvent])(ids = OpticIds.generator)
    flattenedCommands.foreach(command => {
      val result = Try(service.handleCommand(simulatedId, command, commandContext))
      if (result.isFailure) {
        Logger.log(command)
        //        Logger.log(result)
        throw result.failed.get
      }
    })

    val rfcState = service.currentState(simulatedId)
    val resolvers = new DefaultShapesResolvers(rfcState)
    new DiffPreviewer(resolvers, rfcState).previewShape(Some(shapeId)).map(preview => (flattenedCommands, preview))
  }

}


class ExampleRenderVisitorNew(resolvers: ShapesResolvers, spec: RfcState, diffs: Set[ShapeDiffResult], relatedDiffs: Map[ShapeDiffResult, Set[JsonTrail]]) extends JsonLikeAndSpecVisitors with ExampleRenderVisitorHelper {

  def diffsByTrail(bodyTrail: JsonTrail): Set[DiffResult] = {
    val matching = diffs.collect {
      case sd: ShapeDiffResult if sd.jsonTrail == bodyTrail => sd
    }
    //add back in groupings
    matching.toSet ++ Set(relatedDiffs.filter(i => diffs.contains(i._1)).find(i => i._2.contains(bodyTrail)).map(i => i._1)).flatten
  }

  override val objectVisitor: JlasObjectVisitor = new JlasObjectVisitor {
    val diffVisitor = new JsonLikeAndSpecDiffObjectVisitor(resolvers, spec, (_) => Unit, (_) => Unit)

    override def visit(json: JsonLike, jsonTrail: JsonTrail, trailOrigin: ShapeTrail, trailChoices: Seq[ChoiceOutput], itemChoiceCallback: ObjectKeyChoiceCallback): Unit = {
      diffVisitor.visit(json, jsonTrail, trailOrigin, trailChoices, (matches, getChoicesForKey) => {
        // assuming one or zero choices
        if (matches.nonEmpty) {
          val objectMatch = matches.head

          def idFromName(name: String) = jsonTrail.withChild(JsonObjectKey(name)).toString

          val expected = resolvers.resolveTrailToCoreShape(objectMatch.shapeTrail(), Map.empty)
          val observedFields = json.fields

          val fieldNameToId = expected.shapeEntity.descriptor.fieldOrdering
            .map(fieldId => {
              val field = spec.shapesState.fields(fieldId)
              val fieldShape = resolvers.resolveFieldToShape(fieldId, expected.bindings).flatMap(x => {
                Some(x.shapeEntity)
              }).get
              (field.descriptor.name -> (idFromName(field.descriptor.name), field, fieldShape))
            }).toMap

          val missingFieldIds = fieldNameToId.flatMap(entry => {
            val (fieldName, (fieldId, field, fieldShape)) = entry
            val fieldTrail = jsonTrail.withChild(JsonObjectKey(fieldName))
            if (!observedFields.contains(fieldName)) {
              pushField(
                MissingExampleField(fieldTrail.toString, fieldName, fieldId, fieldShape.shapeId, diffs = diffsByTrail(fieldTrail))
              )
              Some(fieldTrail.toString)
            } else None
          })

          val knownFieldsIds = fieldNameToId.flatMap(entry => {
            val (fieldName, (fieldId, field, fieldShape)) = entry
            if (observedFields.contains(fieldName)) {
              val fieldTrail = jsonTrail.withChild(JsonObjectKey(fieldName))
              val jsonValue = observedFields(fieldName)
              pushField(
                KnownExampleField(fieldTrail.toString, fieldName, field.fieldId, fieldShape.shapeId, jsonValue.asJson, diffs = diffsByTrail(fieldTrail))
              )
              Some(fieldTrail.toString)
            } else None
          })

          val extraFieldIds = observedFields.flatMap { case (key, value) => {
            if (!fieldNameToId.contains(key)) {
              Logger.log(s"object has extra field ${key}")
              val fieldTrail = jsonTrail.withChild(JsonObjectKey(key))
              val extraFieldId = fieldTrail.toString

              pushField(
                UnexpectedExampleField(fieldTrail.toString, key, value.asJson, diffs = diffsByTrail(fieldTrail))
              )

              Some(extraFieldId)
            } else None
          }
          }

          pushShape(ExampleObject(
            jsonTrail.toString,
            Some(expected.shapeEntity.shapeId),
            knownFieldsIds.toSeq,
            missingFieldIds.toSeq,
            extraFieldIds.toSeq,
            diffs = diffsByTrail(jsonTrail)
          ))

        } else {
          //for unknown
          val objectId = jsonTrail.toString
          val objectFields = json.fields

          val fieldIds = objectFields.map { case (key, value) => {
            val fieldTrail = jsonTrail.withChild(JsonObjectKey(key))
            val fieldId = fieldTrail.toString
            pushField(UnexpectedExampleField(fieldId, key, value.asJson, Set.empty))
            fieldId
          }
          }.toSeq

          pushShape(
            ExampleObject(
              objectId,
              None,
              Seq.empty,
              Seq.empty,
              unexpectedFieldIds = fieldIds,
              Set.empty
            )
          )
        }
        itemChoiceCallback(matches, getChoicesForKey)
      })
    }
  }
  override val arrayVisitor: JlasArrayVisitor = new JlasArrayVisitor {
    val diffVisitor = new JsonLikeAndSpecDiffArrayVisitor(resolvers, spec, _ => Unit, _ => Unit)

    override def visit(json: JsonLike, jsonTrail: JsonTrail, trailOrigin: ShapeTrail, trailChoices: Seq[ChoiceOutput], itemChoiceCallback: ArrayItemChoiceCallback): Unit = {
      diffVisitor.visit(json, jsonTrail, trailOrigin, trailChoices, (matches) => {
        val wasTheListMatched = matches.nonEmpty
        if (wasTheListMatched) {
          val expected = resolvers.resolveTrailToCoreShape(trailOrigin, Map.empty)
          val resolvedListItem = matches.head

          val ids = json.items.zipWithIndex.map {
            case (i, index) => {
              val itemTrail = jsonTrail.withChild(JsonArrayItem(index))
              pushItem(ExampleItem(
                itemTrail.toString,
                index.intValue(),
                i.asJson,
                diffs = diffsByTrail(itemTrail)
              ))
              itemTrail.toString
            }
          }

          pushShape(ExampleArray(
            jsonTrail.toString,
            Some(expected.shapeEntity.shapeId),
            Some(resolvedListItem.shapeId),
            ids,
            diffs = diffsByTrail(jsonTrail)
          ))

        } else {
          val arrayId = jsonTrail.toString
          val ids = json.items.zipWithIndex.map {
            case (i, index) => {
              val id = jsonTrail.withChild(JsonArrayItem(index)).toString
              pushItem(ExampleItem(
                id,
                index.intValue(),
                i.asJson,
                Set.empty
              ))
              id
            }
          }

          pushShape(ExampleArray(
            arrayId,
            None,
            None,
            ids,
            Set.empty
          ))

        }
        itemChoiceCallback(matches)
      })
    }
  }
  override val objectKeyVisitor: JlasObjectKeyVisitor = new JlasObjectKeyVisitor {
    override def visit(objectJsonTrail: JsonTrail, objectKeys: Map[String, JsonLike], objectChoices: Seq[ChoiceOutput]): Unit = {
      //redundant
    }
  }
  override val primitiveVisitor: JlasPrimitiveVisitor = new JlasPrimitiveVisitor {
    override def visit(json: JsonLike, jsonTrail: JsonTrail, trailOrigin: ShapeTrail, trailChoices: Seq[ChoiceOutput]): Unit = {

      val choicesGroupedByMatch = (
        if (json.isBoolean) {
          trailChoices.groupBy(choice => {
            choice.coreShapeKind match {
              case BooleanKind => true
              case _ => false
            }
          })
        }
        else if (json.isNumber) {
          trailChoices.groupBy(choice => {
            choice.coreShapeKind match {
              case NumberKind => true
              case _ => false
            }
          })
        }
        else if (json.isString) {
          trailChoices.groupBy(choice => {
            choice.coreShapeKind match {
              case StringKind => true
              case _ => false
            }
          })
        }
        else if (json.isNull) {
          trailChoices.groupBy(choice => {
            choice.coreShapeKind match {
              case NullableKind => true
              case _ => false
            }
          })
        }
        else {
          throw new Error("expected json to be a boolean, number, string, or null")
        })

      val matched = choicesGroupedByMatch.getOrElse(true, Seq.empty)

      if (matched.nonEmpty) {
        val firstMatch = matched.head
        Try(resolvers.resolveTrailToCoreShape(firstMatch.shapeTrail(), Map.empty)).toOption //@TODO: check bindings
          .foreach(resolvedTrail => {
            val shape = resolvedTrail.shapeEntity
            val baseShapeId = resolvedTrail.coreShapeKind.baseShapeId

            pushShape(ExamplePrimitive(
              jsonTrail.toString,
              baseShapeId,
              Some(shape.shapeId),
              json.asJson,
              diffs = diffsByTrail(jsonTrail)
            ))
          })
      } else {
        val baseShapeId = JsonLikeResolvers.jsonToCoreKind(json).baseShapeId
        pushShape(ExamplePrimitive(
          jsonTrail.toString,
          baseShapeId,
          None,
          json.asJson,
          diffs = diffsByTrail(jsonTrail)
        ))
      }
    }
  }
}

class ShapeRenderVisitor(resolvers: ShapesResolvers, spec: RfcState, diffs: Set[ShapeDiffResult]) extends ShapeVisitors with SpecRenderVisitorHelper {

  def diffsByTrail(shapeTrail: ShapeTrail): Set[DiffResult] = {
//    diffs.collect {
//      case sd: ShapeDiffResult if sd.shapeTrail == shapeTrail => sd
//    }
    Set.empty
  }


  override val objectVisitor: ObjectShapeVisitor = new ObjectShapeVisitor {
    override def begin(objectResolved: ResolvedTrail, shapeTrail: ShapeTrail, exampleJson: Option[JsonLike]): Unit = {

      val expectedFields = objectResolved.shapeEntity.descriptor.fieldOrdering.flatMap(fieldId => {
        val field = spec.shapesState.fields(fieldId)
        if (field.isRemoved) {
          None
        } else {
          //@GOTCHA need field bindings?
          val fieldShape = resolvers.resolveFieldToShape(fieldId, objectResolved.bindings).get
          Some(
            SpecField(field.descriptor.name, field.fieldId, fieldShape.shapeEntity.shapeId,
              diffsByTrail(
                shapeTrail.withChild(ObjectFieldTrail(field.fieldId, fieldShape.shapeEntity.shapeId))
              ))
          )
        }
      })

      pushShape(
        SpecObject(
          objectResolved.shapeEntity.shapeId,
          expectedFields,
          diffsByTrail(shapeTrail)
        )
      )

    }

    override def visit(key: String, fieldId: FieldId, fieldShapeTrail: ResolvedTrail, fieldTrail: ShapeTrail): Unit = {
      //      val example = exampleFields.get(fieldId).flatMap(_.exampleValue)
      //      pushField(RenderField(
      //        fieldId,
      //        None,
      //        key,
      //        Some(fieldShapeTrail.shapeEntity.shapeId),
      //        example,
      //        diffs = diffsByTrail(fieldTrail)
      //      ))
    }

    override def end(): Unit = ???

  }
  override val listVisitor: ListShapeVisitor = new ListShapeVisitor {

    override def begin(shapeTrail: ShapeTrail, listShape: ShapeEntity, itemShape: ShapeEntity): Unit = {

      val baseItem = resolvers.resolveToBaseShape(itemShape.shapeId)


      pushShape(
        SpecArray(
          listShape.shapeId,
          itemShape.shapeId,
          RenderName(Seq(NameComponent("List of ", ListKind.color, inner = Some(baseItem.shapeId), link = Some(baseItem.shapeId)))),
          diffsByTrail(shapeTrail.withChild(ListItemTrail(listShape.shapeId, itemShape.shapeId)))
        )
      )
    }

    override def end(): Unit = ???


    override def visit(): Unit = ???
  }
  override val primitiveVisitor: PrimitiveShapeVisitor = new PrimitiveShapeVisitor {
    override def visit(objectResolved: ResolvedTrail, shapeTrail: ShapeTrail): Unit = {

      val name = {
        objectResolved.coreShapeKind match {
          case ShapesHelper.AnyKind => RenderName(Seq(NameComponent(AnyKind.name, AnyKind.color)))
          case ShapesHelper.StringKind => RenderName(Seq(NameComponent(StringKind.name, StringKind.color)))
          case ShapesHelper.NumberKind => RenderName(Seq(NameComponent(NumberKind.name, NumberKind.color)))
          case ShapesHelper.BooleanKind => RenderName(Seq(NameComponent(BooleanKind.name, BooleanKind.color)))
          case _ => RenderName(Seq.empty)
        }
      }

      pushShape(SpecPrimitive(
        objectResolved.shapeEntity.shapeId,
        objectResolved.coreShapeKind.baseShapeId,
        name = name,
        diffs = diffsByTrail(shapeTrail)
      ))
    }
  }
  override val oneOfVisitor: OneOfVisitor = new OneOfVisitor {
    override def begin(shapeTrail: ShapeTrail, oneOfShape: ShapeEntity, branches: Seq[ShapeId]): Unit = {

      val nameComponents = branches.map(branch => {
        NameComponent(if (branches.lastOption.contains(branch) && branches.size > 1) "or " else "", "modifier", endText = if (branches.lastOption.contains(branch)) "" else ", ", inner = Some(branch), link = Some(branch))
      })

      pushShape(SpecOneOf(
        oneOfShape.shapeId,
        RenderName(nameComponents),
        branches,
        diffs = diffsByTrail(shapeTrail),
      ))

    }

    override def visit(shapeTrail: ShapeTrail, oneOfShape: ShapeEntity, branchShape: ShapeEntity): Unit = {
    }

    override def end(): Unit = ???
  }
  override val optionalVisitor: GenericWrapperVisitor = new GenericWrapperVisitor {
    override def begin(shapeTrail: ShapeTrail, shape: ShapeEntity, innerShape: Option[ShapeEntity]): Unit = {
      pushShape(
        WrappedType(
          shape.shapeId,
          OptionalKind.baseShapeId,
          RenderName(Seq(NameComponent("", "modifier", " (optional)", innerShape.map(_.shapeId)))),
          innerShape.get.shapeId,
          diffs = diffsByTrail(shapeTrail)
        )
      )
    }
  }
  override val nullableVisitor: GenericWrapperVisitor = new GenericWrapperVisitor {
    override def begin(shapeTrail: ShapeTrail, shape: ShapeEntity, innerShape: Option[ShapeEntity]): Unit = {
      pushShape(
        WrappedType(
          shape.shapeId,
          NullableKind.baseShapeId,
          RenderName(Seq(NameComponent("", "modifier", " (nullable)", innerShape.map(_.shapeId)))),
          innerShape.get.shapeId,
          diffs = diffsByTrail(shapeTrail)
        )
      )
    }
  }
}
