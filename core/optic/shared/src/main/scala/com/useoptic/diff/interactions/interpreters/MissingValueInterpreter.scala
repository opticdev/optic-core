package com.useoptic.diff.interactions.interpreters

import com.useoptic.contexts.rfc.RfcState
import com.useoptic.contexts.shapes.Commands._
import com.useoptic.contexts.shapes.{Commands, ShapesAggregate}
import com.useoptic.contexts.shapes.ShapesHelper.{ListKind, NullableKind, OneOfKind, OptionalKind}
import com.useoptic.diff.initial.{DistributionAwareShapeBuilder, ShapeBuildingStrategy}
import com.useoptic.diff.interactions.interpretations.BasicInterpretations
import com.useoptic.diff.{ChangeType, InteractiveDiffInterpretation}
import com.useoptic.diff.interactions._
import com.useoptic.diff.interactions.interpreters.copy.{NullableShapeTemplates, ObjectSuggestionTemplates, OneOfTemplates}
import com.useoptic.diff.interpreters.InteractiveDiffInterpreter
import com.useoptic.diff.shapes._
import com.useoptic.diff.shapes.resolvers.JsonLikeResolvers
import com.useoptic.dsa.OpticDomainIds
import com.useoptic.logging.Logger
import com.useoptic.types.capture.HttpInteraction
import com.useoptic.ux.ShapeNameRenderer

class MissingValueInterpreter(rfcState: RfcState)(implicit ids: OpticDomainIds) extends InteractiveDiffInterpreter[InteractionDiffResult] {

  private val basicInterpretations = new BasicInterpretations(rfcState)
  private val descriptionInterpreters = new DiffDescriptionInterpreters(rfcState)
  implicit val shapeBuildingStrategy = ShapeBuildingStrategy.learnASingleInteraction
  private val namer = new ShapeNameRenderer(rfcState)

  override def interpret(diff: InteractionDiffResult, interaction: HttpInteraction): Seq[InteractiveDiffInterpretation] = {
    diff match {
      case d: UnmatchedRequestBodyShape => {
        d.shapeDiffResult match {
          case sd: UnmatchedShape => {
            interpretUnmatchedShape(d.interactionTrail, d.requestsTrail, sd.jsonTrail, sd.shapeTrail, interaction)
          }
          case _ => Seq.empty
        }
      }
      case d: UnmatchedResponseBodyShape => {
        d.shapeDiffResult match {
          case sd: UnmatchedShape => {
            interpretUnmatchedShape(d.interactionTrail, d.requestsTrail, sd.jsonTrail, sd.shapeTrail, interaction)
          }
          case _ => Seq.empty

        }
      }
      case _ => Seq.empty
    }
  }


  def interpretUnmatchedShape(interactionTrail: InteractionTrail, requestsTrail: RequestSpecTrail, jsonTrail: JsonTrail, shapeTrail: ShapeTrail, interaction: HttpInteraction): Seq[InteractiveDiffInterpretation] = {
    val resolved = JsonLikeResolvers.tryResolveJson(interactionTrail, jsonTrail, interaction)
    if (resolved.isEmpty) {
      Seq(
        WrapWithOptional(interactionTrail, requestsTrail, jsonTrail, shapeTrail, interaction),
        RemoveFromSpec(interactionTrail, requestsTrail, jsonTrail, shapeTrail, interaction)
      )
    } else {
      if (resolved.get.isNull) {
        Seq(
          WrapWithNullable(interactionTrail, requestsTrail, jsonTrail, shapeTrail, interaction),
        )
      } else {
        shapeTrail.path.lastOption match {
          case Some(t: NullableTrail) => {
            SetNullableInnerShape(interactionTrail, requestsTrail, jsonTrail, shapeTrail, interaction) ++
              Seq(basicInterpretations.ChangeShape(interactionTrail, requestsTrail, shapeTrail, jsonTrail, interaction))
          }
          case _ => {
            Seq(
              WrapWithOneOf(interactionTrail, requestsTrail, jsonTrail, shapeTrail, interaction),
              basicInterpretations.ChangeShape(interactionTrail, requestsTrail, shapeTrail, jsonTrail, interaction),
            )
          }
        }
      }
    }
  }

  def SetNullableInnerShape(interactionTrail: InteractionTrail, requestsTrail: RequestSpecTrail, jsonTrail: JsonTrail, shapeTrail: ShapeTrail, interaction: HttpInteraction): Seq[InteractiveDiffInterpretation] = {
    shapeTrail.path.lastOption match {
      case Some(t: NullableTrail) => {
        val json = JsonLikeResolvers.tryResolveJsonLike(
          interactionTrail,
          jsonTrail,
          interaction
        )
        val (inlineShapeId, newCommands, name) =
          DistributionAwareShapeBuilder.toCommandsWithName(Vector(json.get))

        val commands = newCommands.flatten ++ Seq(
          SetParameterShape(
            ProviderInShape(
              t.shapeId,
              ShapeProvider(inlineShapeId),
              NullableKind.innerParam
            )
          )
        )

        Seq(
          InteractiveDiffInterpretation(
            NullableShapeTemplates.assignNullable(name),
            commands,
            ChangeType.Update
          )
        )
      }
      case _ => Seq.empty
    }
  }

  def RemoveFromSpec(interactionTrail: InteractionTrail, requestsTrail: RequestSpecTrail, jsonTrail: JsonTrail, shapeTrail: ShapeTrail, interaction: HttpInteraction): InteractiveDiffInterpretation = {

    //if this fails we got other problems
    val fieldId = shapeTrail.path.lastOption match {
      case Some(value) => value match {
        case t: ObjectFieldTrail => t.fieldId
      }
    }

    val commands = Seq(Commands.RemoveField(fieldId))

    val fieldName = rfcState.shapesState.flattenedField(fieldId).name

    InteractiveDiffInterpretation(
      ObjectSuggestionTemplates.removeField(fieldName),
      commands,
      ChangeType.Removal
    )
  }

  def WrapWithOptional(interactionTrail: InteractionTrail, requestsTrail: RequestSpecTrail, jsonTrail: JsonTrail, shapeTrail: ShapeTrail, interaction: HttpInteraction): InteractiveDiffInterpretation = {
    val wrapperShapeId = ids.newShapeId
    val baseCommands = Seq(
      AddShape(wrapperShapeId, OptionalKind.baseShapeId, ""),
    )
    val (additionalCommands, fieldName) = shapeTrail.path.lastOption match {
      case Some(pc: ObjectFieldTrail) => {
        val field = rfcState.shapesState.flattenedField(pc.fieldId)
        val fieldName = field.name
        (Seq(
          SetParameterShape(
            ProviderInShape(
              wrapperShapeId,
              field.fieldShapeDescriptor match {
                case fs: FieldShapeFromShape => ShapeProvider(fs.shapeId)
                case fs: FieldShapeFromParameter => ParameterProvider(fs.shapeParameterId)
              },
              OptionalKind.innerParam
            )
          ),
          SetFieldShape(FieldShapeFromShape(field.fieldId, wrapperShapeId)),
        ), fieldName)
      }
    }

    val commands = baseCommands ++ additionalCommands

    InteractiveDiffInterpretation(
      ObjectSuggestionTemplates.makeFieldOptional(fieldName),
      commands,
      ChangeType.Update
    )
  }

  def WrapWithNullable(interactionTrail: InteractionTrail, requestsTrail: RequestSpecTrail, jsonTrail: JsonTrail, shapeTrail: ShapeTrail, interaction: HttpInteraction): InteractiveDiffInterpretation = {
    val wrapperShapeId = ids.newShapeId
    val baseCommands = Seq(
      AddShape(wrapperShapeId, NullableKind.baseShapeId, ""),
    )
    val (additionalCommands, location) = shapeTrail.path.lastOption match {
      case Some(pc: ListItemTrail) => {
        (Seq(
          SetParameterShape(
            ProviderInShape(
              wrapperShapeId,
              ShapeProvider(pc.itemShapeId),
              NullableKind.innerParam
            )
          ),
          SetParameterShape(
            ProviderInShape(
              pc.listShapeId,
              ShapeProvider(wrapperShapeId),
              ListKind.innerParam
            )
          )
        ), "list item")
      }
      case Some(pc: ObjectFieldTrail) => {
        val field = rfcState.shapesState.flattenedField(pc.fieldId)
        (Seq(
          SetParameterShape(
            ProviderInShape(
              wrapperShapeId,
              field.fieldShapeDescriptor match {
                case fs: FieldShapeFromShape => ShapeProvider(fs.shapeId)
                case fs: FieldShapeFromParameter => ParameterProvider(fs.shapeParameterId)
              },
              NullableKind.innerParam
            )
          ),
          SetFieldShape(FieldShapeFromShape(field.fieldId, wrapperShapeId)),
        ), field.name)
      }
    }

    val commands = baseCommands ++ additionalCommands

    InteractiveDiffInterpretation(
      NullableShapeTemplates.makeNullable(location),
      commands,
      ChangeType.Update
    )
  }

  def WrapWithOneOf(interactionTrail: InteractionTrail, requestsTrail: RequestSpecTrail, jsonTrail: JsonTrail, shapeTrail: ShapeTrail, interaction: HttpInteraction): InteractiveDiffInterpretation = {
    val resolved = JsonLikeResolvers.tryResolveJsonLike(interactionTrail, jsonTrail, interaction)
    val wrapperShapeId = ids.newShapeId
    val p1 = ids.newShapeParameterId
    val p2 = ids.newShapeParameterId
    val (inlineShapeId, newCommands, t2Name) = DistributionAwareShapeBuilder.toCommandsWithName(Vector(resolved.get))

    val baseCommands = newCommands.flatten ++ Seq(
      AddShape(wrapperShapeId, OneOfKind.baseShapeId, ""),
      AddShapeParameter(p1, wrapperShapeId, ""),
      AddShapeParameter(p2, wrapperShapeId, ""),
      SetParameterShape(ProviderInShape(wrapperShapeId, ShapeProvider(inlineShapeId), p2))
    )

    val additionalCommands = shapeTrail.path.lastOption match {
      case Some(pc: ObjectFieldTrail) => {
        Seq(
          SetParameterShape(ProviderInShape(wrapperShapeId, ShapeProvider(pc.fieldShapeId), p1)),
          SetFieldShape(FieldShapeFromShape(pc.fieldId, wrapperShapeId))
        )
      }
      case Some(pc: ListItemTrail) => {
        Seq(
          SetParameterShape(ProviderInShape(wrapperShapeId, ShapeProvider(pc.itemShapeId), p1)),
          SetParameterShape(ProviderInShape(pc.listShapeId, ShapeProvider(wrapperShapeId), ListKind.innerParam))
        )
      }
      case Some(pc: OneOfItemTrail) => {
        Logger.log("sentinel-OneOfItemTrail")
        Logger.log(pc)
        Seq(
          SetParameterShape(ProviderInShape(wrapperShapeId, ShapeProvider(pc.itemShapeId), p1)),
          SetParameterShape(ProviderInShape(pc.oneOfId, ShapeProvider(wrapperShapeId), pc.parameterId))
        )
      }
      case x => {
        Logger.log(x)
        Seq.empty
      }
    }
    val commands = baseCommands ++ additionalCommands

    val identifier = descriptionInterpreters.jsonTrailDetailedDescription(jsonTrail)

    val t1Name = shapeTrail.path.lastOption match {
      case Some(pc: UnknownTrail) => "Unknown"
      case Some(a: ShapeTrailPathComponent) => {
        namer.nameForShapeId(a.namedShape).get.map(_.text).mkString(" ")
      } case None => {
        namer.nameForShapeId(shapeTrail.rootShapeId).get.map(_.text).mkString(" ")
      }
    }

    InteractiveDiffInterpretation(
      OneOfTemplates.makeOneOf(identifier, t1Name, t2Name),
      commands,
      ChangeType.Addition
    )
  }

  def AddToOneOf(interactionTrail: InteractionTrail): InteractiveDiffInterpretation = {

    //@todo

    InteractiveDiffInterpretation(
      OneOfTemplates.addToOneOf(),
      Seq(),
      ChangeType.Addition
    )
  }

  override def interpret(diff: InteractionDiffResult, interactions: Vector[HttpInteraction]): Seq[InteractiveDiffInterpretation] = interpret(diff, interactions.head)
}
