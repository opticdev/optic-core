package com.useoptic.diff.interactions.interpreters

import com.useoptic.contexts.requests.Commands.ShapedBodyDescriptor
import com.useoptic.contexts.rfc.RfcState
import com.useoptic.contexts.requests.{RequestsServiceHelper, Commands => RequestsCommands}
import com.useoptic.contexts.rfc.Commands.RfcCommand
import com.useoptic.contexts.shapes.Commands.ShapeId
import com.useoptic.diff.{ChangeType, InteractiveDiffInterpretation}
import com.useoptic.diff.initial.{DistributionAwareShapeBuilder, ShapeBuildingStrategy}
import com.useoptic.diff.interactions.{BodyUtilities, InteractionDiffResult, InteractionTrail, RequestSpecTrail, RequestSpecTrailHelpers, UnmatchedRequestBodyContentType, UnmatchedResponseBodyContentType, UnmatchedResponseStatusCode}
import com.useoptic.diff.shapes.JsonTrail
import com.useoptic.diff.shapes.resolvers.{JsonLikeResolvers, ShapesResolvers}
import com.useoptic.dsa.OpticDomainIds
import com.useoptic.types.capture.HttpInteraction
import com.useoptic.ux.{DiffPreviewer, ShapeOnlyRenderHelper}

class InitialBodyInterpreter(rfcState: RfcState)(implicit ids: OpticDomainIds) {

  val diffPreviewer = new DiffPreviewer(ShapesResolvers.newResolver(rfcState), rfcState)

  private def inRequest(diff: InteractionDiffResult) = diff match {
    case _:UnmatchedRequestBodyContentType => true
    case _:UnmatchedResponseBodyContentType => false
    case _:UnmatchedResponseStatusCode => false
  }
  private def inResponse(diff: InteractionDiffResult) = diff match {
    case _:UnmatchedRequestBodyContentType => false
    case _:UnmatchedResponseBodyContentType => true
    case _:UnmatchedResponseStatusCode => true
  }


  def interpret(diff: InteractionDiffResult, inferPolymorphism: Boolean, interactions: Vector[HttpInteraction]) = {
    require(interactions.nonEmpty, "There must be at least one interaction associated with a diff")

    def getBody(i: HttpInteraction) = {
      if (inRequest(diff)) {
        Some(i.request.body)
      } else if (inResponse(diff)) {
        Some(i.response.body)
      } else {
        None
      }
    }

    if (inferPolymorphism) {
      implicit val shapeBuildingStrategy = ShapeBuildingStrategy.inferPolymorphism
      val bodies = interactions.flatMap(getBody).flatMap(BodyUtilities.parseBody)
      diffPreviewer.shapeOnlyFromShapeBuilder(bodies).map {
        case (commands, shapeRender) => {

          val suggestion = if (inRequest(diff)) {
            AddRequestContentType(diff.interactionTrail, diff.requestsTrail, interactions.head, commands, shapeRender.getRootShape.get.baseShapeId)
          } else {
            AddResponseContentType(diff.interactionTrail, diff.requestsTrail, interactions.head, commands, shapeRender.getRootShape.get.baseShapeId)
          }

          InitialBodyInterpretation(shapeRender, suggestion)
        }
      }
    } else {
      implicit val shapeBuildingStrategy = ShapeBuildingStrategy.learnASingleInteraction
      val bodies = interactions.headOption.flatMap(getBody).flatMap(BodyUtilities.parseBody).toVector
      diffPreviewer.shapeOnlyFromShapeBuilder(bodies).map {
        case (commands, shapeRender) => {
          val suggestion = if (inRequest(diff)) {
            AddRequestContentType(diff.interactionTrail, diff.requestsTrail, interactions.head, commands, shapeRender.getRootShape.get.baseShapeId)
          } else {
            AddResponseContentType(diff.interactionTrail, diff.requestsTrail, interactions.head, commands, shapeRender.getRootShape.get.baseShapeId)
          }

          InitialBodyInterpretation(shapeRender, suggestion)
        }
      }
    }

  }

  def AddRequestContentType(interactionTrail: InteractionTrail, requestsTrail: RequestSpecTrail, anInteraction: HttpInteraction, shapeCommands: Vector[RfcCommand], rootShapeId: ShapeId): InteractiveDiffInterpretation = {
    val requestId = ids.newRequestId
    val pathId = RequestSpecTrailHelpers.pathId(requestsTrail).get
    val baseCommands = Seq(
      RequestsCommands.AddRequest(requestId, pathId, anInteraction.request.method),
    )
    interactionTrail.requestBodyContentTypeOption() match {
      case Some(contentType) => {
        val jsonBody = JsonLikeResolvers.tryResolveJsonLike(interactionTrail, JsonTrail(Seq()), anInteraction)
        val actuallyHasBody = jsonBody.isDefined
        if (actuallyHasBody) {
          val commands = baseCommands ++ shapeCommands ++ Seq(
            RequestsCommands.SetRequestBodyShape(requestId, ShapedBodyDescriptor(contentType, rootShapeId, isRemoved = false))
          )

          InteractiveDiffInterpretation(
            s"Add Request with ${contentType} Body",
            s"Added Request with ${contentType} Body",
            commands,
            ChangeType.Addition
          )
        } else {
          val commands = baseCommands
          InteractiveDiffInterpretation(
            s"Add Request with ${contentType} Content-Type but no Body",
            s"Added Request with ${contentType} Content-Type but no Body",
            commands,
            ChangeType.Addition
          )
        }

      }
      case None => {
        val commands = baseCommands
        InteractiveDiffInterpretation(
          s"Add Request with No Body",
          s"Added Request with No Body",
          commands,
          ChangeType.Addition
        )
      }
    }
  }

  def AddResponseContentType(interactionTrail: InteractionTrail, requestsTrail: RequestSpecTrail, anInteraction: HttpInteraction, shapeCommands: Vector[RfcCommand], rootShapeId: ShapeId) = {
    val responseId = ids.newResponseId
    val pathId = RequestSpecTrailHelpers.pathId(requestsTrail).get
    val baseCommands = Seq(
      RequestsCommands.AddResponseByPathAndMethod(responseId, pathId, anInteraction.request.method, anInteraction.response.statusCode),
    )
    interactionTrail.responseBodyContentTypeOption() match {
      case Some(contentType) => {
        val jsonBody = JsonLikeResolvers.tryResolveJsonLike(interactionTrail, JsonTrail(Seq()), anInteraction)
        val actuallyHasBody = jsonBody.isDefined
        if (actuallyHasBody) {

          val commands = baseCommands ++ shapeCommands ++ Seq(
            RequestsCommands.SetResponseBodyShape(responseId, ShapedBodyDescriptor(contentType, rootShapeId, isRemoved = false))
          )

          InteractiveDiffInterpretation(
            s"Add ${interactionTrail.statusCode()} Response with ${contentType} Body",
            s"Added ${interactionTrail.statusCode()} Response with ${contentType} Body",
            commands,
            ChangeType.Addition
          )
        } else {
          val commands = baseCommands

          InteractiveDiffInterpretation(
            s"Add ${interactionTrail.statusCode()} Response with ${contentType} Content-Type but no Body",
            s"Added ${interactionTrail.statusCode()} Response with ${contentType} Content-Type but no Body",
            commands,
            ChangeType.Addition
          )
        }
      }
      case None => {
        val commands = baseCommands
        InteractiveDiffInterpretation(
          s"Add ${interactionTrail.statusCode()} Response with No Body",
          s"Added ${interactionTrail.statusCode()} Response with No Body",
          commands,
          ChangeType.Addition
        )
      }
    }
  }


}


case class InitialBodyInterpretation(shape: ShapeOnlyRenderHelper, suggestion: InteractiveDiffInterpretation)
