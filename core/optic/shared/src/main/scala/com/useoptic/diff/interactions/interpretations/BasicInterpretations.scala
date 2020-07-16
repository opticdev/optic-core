package com.useoptic.diff.interactions.interpretations

import com.useoptic.contexts.requests.Commands.ShapedBodyDescriptor
import com.useoptic.contexts.requests.{RequestsServiceHelper, Commands => RequestsCommands}
import com.useoptic.contexts.rfc.Commands.RfcCommand
import com.useoptic.contexts.rfc.RfcState
import com.useoptic.contexts.shapes.Commands.{FieldShapeFromShape, ProviderInShape, ShapeProvider}
import com.useoptic.contexts.shapes.ShapesHelper.{ListKind, ObjectKind}
import com.useoptic.contexts.shapes.{ShapesAggregate, ShapesHelper, Commands => ShapesCommands}
import com.useoptic.diff.{ChangeType, InteractiveDiffInterpretation}
import com.useoptic.diff.initial.{DistributionAwareShapeBuilder, ShapeBuildingStrategy}
import com.useoptic.diff.interactions.interpreters.DiffDescriptionInterpreters
import com.useoptic.diff.interactions.{BodyUtilities, InteractionTrail, RequestSpecTrail, RequestSpecTrailHelpers}
import com.useoptic.diff.shapes.{JsonTrail, ListItemTrail, ListTrail, ObjectFieldTrail, ObjectTrail, OneOfItemTrail, ShapeTrail, UnknownTrail}
import com.useoptic.diff.shapes.JsonTrailPathComponent._
import com.useoptic.diff.shapes.resolvers.JsonLikeResolvers
import com.useoptic.dsa.OpticDomainIds
import com.useoptic.logging.Logger
import com.useoptic.types.capture.{HttpInteraction, JsonLikeFrom}
import com.useoptic.diff.shapes.OptionalItemTrail
import com.useoptic.diff.shapes.OptionalTrail
import com.useoptic.contexts.shapes.ShapesHelper.OptionalKind
import com.useoptic.diff.interactions.interpreters.copy.{ChangeShapeSuggestionTemplates, NewBodiesSuggestionTemplates, ObjectSuggestionTemplates}
import com.useoptic.diff.shapes.NullableItemTrail
import com.useoptic.diff.shapes.NullableTrail
import com.useoptic.diff.shapes.OneOfTrail

class BasicInterpretations(rfcState: RfcState)(implicit ids: OpticDomainIds) {

  private val descriptionInterpreters = new DiffDescriptionInterpreters(rfcState)
  implicit val shapeBuildingStrategy = ShapeBuildingStrategy.learnASingleInteraction

  def AddResponse(interactionTrail: InteractionTrail, requestsTrail: RequestSpecTrail): InteractiveDiffInterpretation = {
    val requestId = RequestSpecTrailHelpers.requestId(requestsTrail).get

    val responseId = ids.newResponseId
    val commands = Seq(
      RequestsCommands.AddResponse(responseId, requestId, interactionTrail.statusCode())
    )
    InteractiveDiffInterpretation(
      NewBodiesSuggestionTemplates.addResponseType(interactionTrail.statusCode(), interactionTrail.responseBodyContentTypeOption(), interactionTrail.responseBodyContentTypeOption().isDefined),
      commands,
      ChangeType.Addition
    )
  }

  def AddRequestContentType(interactionTrail: InteractionTrail, requestsTrail: RequestSpecTrail, interactions: Vector[HttpInteraction]): InteractiveDiffInterpretation = {
    val baseInteraction = interactions.head
    val requestId = ids.newRequestId
    val pathId = RequestSpecTrailHelpers.pathId(requestsTrail).get
    val baseCommands = Seq(
      RequestsCommands.AddRequest(requestId, pathId, baseInteraction.request.method),
    )
    interactionTrail.requestBodyContentTypeOption() match {
      case Some(contentType) => {
        val jsonBody = JsonLikeResolvers.tryResolveJsonLike(interactionTrail, JsonTrail(Seq()), baseInteraction)
        val actuallyHasBody = jsonBody.isDefined
        if (actuallyHasBody) {

          implicit val shapeBuildingStrategy = if (interactions.size > 1) ShapeBuildingStrategy.inferPolymorphism else ShapeBuildingStrategy.learnASingleInteraction

          val (rootShapeId, buildCommands) = DistributionAwareShapeBuilder.toCommands(interactions.flatMap(i =>  BodyUtilities.parseBody(i.request.body)))
          val commands = baseCommands ++ buildCommands.flatten ++ Seq(
            RequestsCommands.SetRequestBodyShape(requestId, ShapedBodyDescriptor(contentType, rootShapeId, isRemoved = false))
          )

          InteractiveDiffInterpretation(
            NewBodiesSuggestionTemplates.addRequestType(contentType, hasBody = true),
            commands,
            ChangeType.Addition
          )
        } else {
          val commands = baseCommands
          InteractiveDiffInterpretation(
            NewBodiesSuggestionTemplates.addRequestType(contentType, hasBody = false),
            commands,
            ChangeType.Addition
          )
        }

      }
      case None => {
        val commands = baseCommands
        InteractiveDiffInterpretation(
          NewBodiesSuggestionTemplates.addRequestNoBody(),
          commands,
          ChangeType.Addition
        )
      }
    }
  }

  def AddResponseContentType(interactionTrail: InteractionTrail, requestsTrail: RequestSpecTrail, interactions: Vector[HttpInteraction]) = {
    val baseInteraction = interactions.head
    val responseId = ids.newResponseId
    val pathId = RequestSpecTrailHelpers.pathId(requestsTrail).get
    val baseCommands = Seq(
      RequestsCommands.AddResponseByPathAndMethod(responseId, pathId, baseInteraction.request.method, baseInteraction.response.statusCode),
    )
    interactionTrail.responseBodyContentTypeOption() match {
      case Some(contentType) => {
        val jsonBody = JsonLikeResolvers.tryResolveJsonLike(interactionTrail, JsonTrail(Seq()), baseInteraction)
        val actuallyHasBody = jsonBody.isDefined
        if (actuallyHasBody) {
          implicit val shapeBuildingStrategy = if (interactions.size > 1) ShapeBuildingStrategy.inferPolymorphism else ShapeBuildingStrategy.learnASingleInteraction
          val (rootShapeId, buildCommands) = DistributionAwareShapeBuilder.toCommands(interactions.flatMap(i =>  BodyUtilities.parseBody(i.response.body)))

          val commands = baseCommands ++ buildCommands.flatten ++ Seq(
            RequestsCommands.SetResponseBodyShape(responseId, ShapedBodyDescriptor(contentType, rootShapeId, isRemoved = false))
          )

          InteractiveDiffInterpretation(
            NewBodiesSuggestionTemplates.addResponseType(interactionTrail.statusCode(), Some(contentType), true),
            commands,
            ChangeType.Addition
          )
        } else {
          val commands = baseCommands

          InteractiveDiffInterpretation(
            NewBodiesSuggestionTemplates.addResponseType(interactionTrail.statusCode(), Some(contentType), false),
            commands,
            ChangeType.Addition
          )
        }
      }
      case None => {
        val commands = baseCommands
        InteractiveDiffInterpretation(
          NewBodiesSuggestionTemplates.addResponseType(interactionTrail.statusCode(), None, false),
          commands,
          ChangeType.Addition
        )
      }
    }
  }

  //@GOTCHA: this is not a backwards-compatible change
  def ChangeShape(interactionTrail: InteractionTrail, requestsTrail: RequestSpecTrail, shapeTrail: ShapeTrail, jsonTrail: JsonTrail, interaction: HttpInteraction): InteractiveDiffInterpretation = {
    val resolved = JsonLikeResolvers.tryResolveJsonLike(interactionTrail, jsonTrail, interaction)

    //@TODO: inject real shapesState? for now this will always create a new shape
    val (inlineShapeId, newCommands, name) = DistributionAwareShapeBuilder.toCommandsWithName(Vector(resolved.get))

    val additionalCommands: Seq[RfcCommand] = shapeTrail.path.lastOption match {
      case Some(trailItem) => trailItem match {
        case t: ObjectTrail => Seq(
          ShapesCommands.SetBaseShape(t.shapeId, inlineShapeId)
        )
        case t: ObjectFieldTrail => Seq(
          ShapesCommands.SetFieldShape(FieldShapeFromShape(t.fieldId, inlineShapeId))
        )
        case t: ListTrail => Seq(
          ShapesCommands.SetBaseShape(t.shapeId, inlineShapeId)
        )
        case t: ListItemTrail => {
          Seq(
            ShapesCommands.SetParameterShape(ProviderInShape(t.listShapeId, ShapeProvider(inlineShapeId), ListKind.innerParam))
          )
        }
        case t: OneOfItemTrail => {
          Logger.log("sentinel-ChangeShape-OneOfItemTrail")
          Seq(
            ShapesCommands.SetParameterShape(ProviderInShape(t.oneOfId, ShapeProvider(inlineShapeId), t.parameterId))
          )
        }
        case t: OptionalTrail => {
          //@TODO
          Seq.empty
        }
        case t: OptionalItemTrail => {
          //@TODO
          Seq(
            ShapesCommands.SetParameterShape(ProviderInShape(t.innerShapeId, ShapeProvider(inlineShapeId), OptionalKind.innerParam))
          )
        }
        case t: NullableTrail => {
          //@TODO:
          Seq.empty
        }
        case t: NullableItemTrail => {
          //@TODO:
          Seq.empty
        }
        case t: OneOfTrail => {
          //@TODO:
          Seq.empty
        }
        case t: OneOfItemTrail => {
          //@TODO:
          Seq.empty
        }
        case t: UnknownTrail => {
          //@TODO:
          Seq.empty
        }
        case _ => {
          Seq.empty
        }
      }
      case None => Seq(
        ShapesCommands.SetBaseShape(shapeTrail.rootShapeId, inlineShapeId)
      )
    }
    val commands = newCommands.flatten ++ additionalCommands

    InteractiveDiffInterpretation(
      ChangeShapeSuggestionTemplates.changeShape(
        //consider changing
        descriptionInterpreters.jsonTrailDetailedDescription(jsonTrail),
        name
      ),
      commands,
      ChangeType.Update
    )
  }

  //@GOTCHA: this is not a backwards-compatible change
  def AddFieldToShape(interactionTrail: InteractionTrail, requestsTrail: RequestSpecTrail, shapeTrail: ShapeTrail, jsonTrail: JsonTrail, interaction: HttpInteraction) = {
    val resolved = JsonLikeResolvers.tryResolveJsonLike(interactionTrail, jsonTrail, interaction)

    //@TODO: inject real shapesState? for now this will always create a new shape
    val (inlineShapeId, newCommands, name) = DistributionAwareShapeBuilder.toCommandsWithName(Vector(resolved.get))
    val fieldId = ids.newFieldId
    val shapeId = shapeTrail.lastObject().get
    val fieldName = jsonTrail.path.last.asInstanceOf[JsonObjectKey].key
    val additionalCommands = Seq(
      ShapesCommands.AddField(fieldId, shapeId, fieldName, FieldShapeFromShape(fieldId, inlineShapeId))
    )

    val commands = newCommands.flatten ++ additionalCommands

    InteractiveDiffInterpretation(
      ObjectSuggestionTemplates.addField(fieldName, name),
      commands,
      ChangeType.Addition
    )
  }
}
