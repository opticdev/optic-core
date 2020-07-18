package com.useoptic.diff.interactions.interpreters

import com.useoptic.contexts.requests.Commands.ShapedBodyDescriptor
import com.useoptic.contexts.rfc.RfcState
import com.useoptic.contexts.requests.{RequestsServiceHelper, Commands => RequestsCommands}
import com.useoptic.contexts.rfc.Commands.RfcCommand
import com.useoptic.contexts.shapes.Commands.ShapeId
import com.useoptic.diff.{ChangeType, InteractiveDiffInterpretation}
import com.useoptic.diff.initial.{DistributionAwareShapeBuilder, ShapeBuildingStrategy}
import com.useoptic.diff.interactions.interpreters.copy.NewBodiesSuggestionTemplates
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


  def interpret(diff: InteractionDiffResult, inferPolymorphism: Boolean, interactions: Vector[HttpInteraction]): InitialBodyInterpretation = {
    require(interactions.nonEmpty, "There must be at least one interaction associated with a diff")

    def buildBaseShape(): Option[(Vector[RfcCommand], ShapeOnlyRenderHelper)] = {

      def getBody(i: HttpInteraction) = {
        if (inRequest(diff)) {
          Some(i.request.body)
        } else if (inResponse(diff)) {
          Some(i.response.body)
        } else {
          None
        }
      }

      implicit val shapeBuildingStrategy = if (inferPolymorphism) ShapeBuildingStrategy.inferPolymorphism else ShapeBuildingStrategy.learnASingleInteraction
      val bodies = interactions.flatMap(getBody).flatMap(BodyUtilities.parseBody)
      val filteredBodies = if (inferPolymorphism) {
        bodies
      } else {
        bodies.take(1)
      }

      diffPreviewer.shapeOnlyFromShapeBuilder(filteredBodies)
    }

    val interactionTrail: InteractionTrail = diff.interactionTrail
    val requestsTrail: RequestSpecTrail = diff.requestsTrail

    def AddRequestContentType(): InitialBodyInterpretation = {
      val requestId = ids.newRequestId
      val anInteraction = interactions.head

      val pathId = RequestSpecTrailHelpers.pathId(requestsTrail).get
      val baseCommands = Seq(
        RequestsCommands.AddRequest(requestId, pathId, anInteraction.request.method),
      )
      interactionTrail.requestBodyContentTypeOption() match {
        case Some(contentType) => {
          val jsonBody = JsonLikeResolvers.tryResolveJsonLike(interactionTrail, JsonTrail(Seq()), anInteraction)
          val actuallyHasBody = jsonBody.isDefined
          if (actuallyHasBody) {
            val baseShape = buildBaseShape().get
            val shapeCommands = baseShape._1
            val rootShapeId = baseShape._2.getRootShape.get.baseShapeId
            val commands = baseCommands ++ shapeCommands ++ Seq(
              RequestsCommands.SetRequestBodyShape(requestId, ShapedBodyDescriptor(contentType, rootShapeId, isRemoved = false))
            )
            InitialBodyInterpretation(
              Some(baseShape._2),
              InteractiveDiffInterpretation(
                NewBodiesSuggestionTemplates.addRequestType(contentType, true),
                commands,
                ChangeType.Addition
              )
            )
          } else {
            val commands = baseCommands
            InitialBodyInterpretation(
              None,
              InteractiveDiffInterpretation(
                NewBodiesSuggestionTemplates.addRequestType(contentType, false),
                commands,
                ChangeType.Addition
              )
            )
          }

        }
        case None => {
          val commands = baseCommands
          InitialBodyInterpretation(
            None,
            InteractiveDiffInterpretation(
              NewBodiesSuggestionTemplates.addRequestNoContentOrBody(),
              commands,
              ChangeType.Addition
          ))
        }
      }
    }

    def AddResponseContentType(): InitialBodyInterpretation = {
      val responseId = ids.newResponseId
      val anInteraction = interactions.head
      val pathId = RequestSpecTrailHelpers.pathId(requestsTrail).get
      val baseCommands = Seq(
        RequestsCommands.AddResponseByPathAndMethod(responseId, pathId, anInteraction.request.method, anInteraction.response.statusCode),
      )
      interactionTrail.responseBodyContentTypeOption() match {
        case Some(contentType) => {
          val jsonBody = JsonLikeResolvers.tryResolveJsonLike(interactionTrail, JsonTrail(Seq()), anInteraction)
          val actuallyHasBody = jsonBody.isDefined
          if (actuallyHasBody) {

            val baseShape = buildBaseShape().get
            val shapeCommands = baseShape._1
            val rootShapeId = baseShape._2.getRootShape.get.baseShapeId

            val commands = baseCommands ++ shapeCommands ++ Seq(
              RequestsCommands.SetResponseBodyShape(responseId, ShapedBodyDescriptor(contentType, rootShapeId, isRemoved = false))
            )

            InitialBodyInterpretation(
              Some(baseShape._2),
              InteractiveDiffInterpretation(
                NewBodiesSuggestionTemplates.addResponseType(interactionTrail.statusCode(), Some(contentType), true),
                commands,
                ChangeType.Addition
              )
            )
          } else {
            val commands = baseCommands

            InitialBodyInterpretation(
              None,
              InteractiveDiffInterpretation(
                NewBodiesSuggestionTemplates.addResponseType(interactionTrail.statusCode(), Some(contentType), false),
                commands,
                ChangeType.Addition
            ))
          }
        }
        case None => {
          val commands = baseCommands
          InitialBodyInterpretation(
            None,
              InteractiveDiffInterpretation(
              NewBodiesSuggestionTemplates.addResponseType(interactionTrail.statusCode(), None, false),
              commands,
              ChangeType.Addition
            )
          )
        }
      }
    }


    if (inRequest(diff)) {
      AddRequestContentType()
    } else {
      AddResponseContentType()
    }

  }

}


case class InitialBodyInterpretation(shape: Option[ShapeOnlyRenderHelper], suggestion: InteractiveDiffInterpretation)
