package com.useoptic.diff.interactions.interpreters

import com.useoptic.contexts.rfc.RfcState
import com.useoptic.contexts.shapes.Commands.{AddField, FieldShapeFromShape, ProviderInShape, SetParameterShape, ShapeProvider}
import com.useoptic.contexts.shapes.{ShapesAggregate, ShapesHelper}
import com.useoptic.contexts.shapes.ShapesHelper.{ListKind, ObjectKind, UnknownKind}
import com.useoptic.diff.initial.{DistributionAwareShapeBuilder, ShapeBuildingStrategy}
import com.useoptic.diff.{ChangeType, InteractiveDiffInterpretation}
import com.useoptic.diff.interactions.{InteractionDiffResult, InteractionTrail, UnmatchedRequestBodyShape, UnmatchedResponseBodyShape}
import com.useoptic.diff.interpreters.InteractiveDiffInterpreter
import com.useoptic.diff.shapes.JsonTrailPathComponent.JsonObjectKey
import com.useoptic.diff.shapes._
import com.useoptic.diff.shapes.resolvers.{JsonLikeResolvers, ShapesResolvers}
import com.useoptic.dsa.OpticDomainIds
import com.useoptic.logging.Logger
import com.useoptic.types.capture.HttpInteraction
import com.useoptic.contexts.shapes.ShapesHelper.OptionalKind
import com.useoptic.contexts.shapes.ShapesHelper.NullableKind
import com.useoptic.diff.interactions.interpreters.copy.{ListTemplates, NullableShapeTemplates, ObjectSuggestionTemplates}

class UnspecifiedShapeDiffInterpreter(
    resolvers: ShapesResolvers,
    rfcState: RfcState
)(implicit ids: OpticDomainIds)
    extends InteractiveDiffInterpreter[InteractionDiffResult] {

  implicit val shapeBuildingStrategy = ShapeBuildingStrategy.learnASingleInteraction

  override def interpret(
      diff: InteractionDiffResult,
      interaction: HttpInteraction
  ): Seq[InteractiveDiffInterpretation] = {
    diff match {
      case d: UnmatchedRequestBodyShape => {
        d.shapeDiffResult match {
          case sd: UnspecifiedShape => {
            interpretUnspecifiedShape(d.interactionTrail, sd, interaction)
          }
          case _ => Seq.empty
        }

      }
      case d: UnmatchedResponseBodyShape => {
        d.shapeDiffResult match {
          case sd: UnspecifiedShape => {
            interpretUnspecifiedShape(d.interactionTrail, sd, interaction)
          }
          case _ => Seq.empty
        }
      }
      case _ => Seq.empty
    }
  }

  def interpretUnspecifiedShape(
      interactionTrail: InteractionTrail,
      shapeDiff: UnspecifiedShape,
      interaction: HttpInteraction
  ): Seq[InteractiveDiffInterpretation] = {
    val choices = resolvers.listTrailChoices(shapeDiff.shapeTrail, Map.empty)
    choices.foreach(Logger.log)
    val choiceWithUnspecifiedShape = choices.find(choice => {
      choice.coreShapeKind match {
        case ObjectKind => true
        case ListKind   => true
        case NullableKind => true
        case _ => false
      }
    })
    choiceWithUnspecifiedShape match {
      case Some(c) => {
        c.coreShapeKind match {
          case ObjectKind => {
            Logger.log(shapeDiff.jsonTrail)
            val json = JsonLikeResolvers.tryResolveJsonLike(
              interactionTrail,
              shapeDiff.jsonTrail,
              interaction
            )
            Logger.log(json.get)
            val key =
              shapeDiff.jsonTrail.path.last.asInstanceOf[JsonObjectKey].key
            val (inlineShapeId, newCommands, fieldShapeName) =
              DistributionAwareShapeBuilder.toCommandsWithName(Vector(json.get))(ids, shapeBuildingStrategy)

            val fieldId = ids.newFieldId
            val commands = newCommands.flatten ++ Seq(
              AddField(
                fieldId,
                c.shapeId,
                key,
                FieldShapeFromShape(fieldId, inlineShapeId)
              )
            )
            Seq(
              InteractiveDiffInterpretation(
                ObjectSuggestionTemplates.addField(key, fieldShapeName),
                commands,
                ChangeType.Addition
              )
            )
          }
          case ListKind => {
            val json = JsonLikeResolvers.tryResolveJsonLike(
              interactionTrail,
              shapeDiff.jsonTrail,
              interaction
            )
            val (inlineShapeId, newCommands, listItemShapeName) =
              DistributionAwareShapeBuilder.toCommandsWithName(Vector(json.get))
            val commands = newCommands.flatten ++ Seq(
              SetParameterShape(
                ProviderInShape(
                  c.shapeId,
                  ShapeProvider(inlineShapeId),
                  ListKind.innerParam
                )
              )
            )
            Seq(
              InteractiveDiffInterpretation(
                ListTemplates.setListItemShape(listItemShapeName),
                commands,
                ChangeType.Update
              )
            )

          }
          case NullableKind => {
            val json = JsonLikeResolvers.tryResolveJsonLike(
              interactionTrail,
              shapeDiff.jsonTrail,
              interaction
            )
            val (inlineShapeId, newCommands, nullableInnerShape) =
              DistributionAwareShapeBuilder.toCommandsWithName(Vector(json.get))

            val commands = newCommands.flatten ++ Seq(
              SetParameterShape(
                ProviderInShape(
                  c.shapeId,
                  ShapeProvider(inlineShapeId),
                  NullableKind.innerParam
                )
              )
            )

            Seq(
              InteractiveDiffInterpretation(
                NullableShapeTemplates.assignNullable(nullableInnerShape),
                commands,
                ChangeType.Update
              )
            )

          }
          case _ => Seq.empty
        }
      }
      case None => Seq.empty
    }

  }

  override def interpret(
      diff: InteractionDiffResult,
      interactions: Vector[HttpInteraction]
  ): Seq[InteractiveDiffInterpretation] = interpret(diff, interactions.head)
}
