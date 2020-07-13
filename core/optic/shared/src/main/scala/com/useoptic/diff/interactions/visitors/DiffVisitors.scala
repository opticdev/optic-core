package com.useoptic.diff.interactions.visitors

import com.useoptic.contexts.requests.Commands._
import com.useoptic.contexts.requests._
import com.useoptic.diff.interactions._
import com.useoptic.diff.shapes.resolvers.ShapesResolvers
import com.useoptic.diff.shapes.{JsonTrail, ShapeDiffResult, ShapeTrail}
import com.useoptic.dsa.Counter
import com.useoptic.logging.Logger
import com.useoptic.types.capture.HttpInteraction

class DiffPathVisitor(emit: (InteractionDiffResult) => Unit) extends PathVisitor {
  def visit(interaction: HttpInteraction, context: PathVisitorContext): Unit = {
    Logger.log("visiting path", interaction.request.path, context.path)
    if (context.path.isEmpty) {
      val interactionTrail = InteractionTrail(Seq())
      val requestsTrail = SpecRoot()
      emit(UnmatchedRequestUrl(interactionTrail, requestsTrail))
    }
  }
}

class DiffRequestBodyVisitor(resolvers: ShapesResolvers, emit: (InteractionDiffResult) => Unit) extends RequestBodyVisitor {
  var visitedWithUnmatchedContentTypes: Set[RequestId] = Set()
  var visitedWithMatchedContentTypes: Set[RequestId] = Set()
  var visitedShapeTrails: Counter[ShapeTrail] = new Counter[ShapeTrail]

  override def begin(): Unit = {
    visitedWithUnmatchedContentTypes = Set()
    visitedWithMatchedContentTypes = Set()
    visitedShapeTrails = new Counter[ShapeTrail]
  }


  override def visit(interaction: HttpInteraction, context: RequestBodyVisitorContext): Unit = {
    if (context.path.isEmpty) {
      return
    }
    if (context.request.isEmpty) {
      return
    }
    context.request match {
      case Some(request) => {
        val actualContentType = ContentTypeHelpers.contentType(interaction.request)
        val expectedContentType = request.requestDescriptor.bodyDescriptor
        // Logger.log(expectedContentType, actualContentType)
        (expectedContentType, actualContentType) match {
          case (expected: UnsetBodyDescriptor, None) => {
            // Logger.log("spec says no body, request has no body")
            visitedWithMatchedContentTypes = visitedWithMatchedContentTypes + request.requestId
          }
          case (expected: UnsetBodyDescriptor, Some(contentTypeHeader)) => {
            // spec says no body, request has body
            // Logger.log("spec says no body, request has body")
            visitedWithUnmatchedContentTypes = visitedWithUnmatchedContentTypes + request.requestId
          }
          case (expected: ShapedBodyDescriptor, None) => {
            // Logger.log("spec says body, request has no body")
            visitedWithUnmatchedContentTypes = visitedWithUnmatchedContentTypes + request.requestId
          }
          case (expected: ShapedBodyDescriptor, Some(contentTypeHeader)) => {
            // Logger.log("spec says body, request has body")
            if (expected.httpContentType == contentTypeHeader) {
              visitedWithMatchedContentTypes = visitedWithMatchedContentTypes + request.requestId


              val diffs = scala.collection.mutable.ArrayBuffer[ShapeDiffResult]()

              def emitShapeDiff(diff: ShapeDiffResult) = {
                diffs.append(diff)
              }

              val visitedShapes = new Counter[ShapeTrail]

              def markShapeTrailAsVisited(trail: ShapeTrail) = {
                visitedShapes.increment(trail)
              }

              val shapeDiffVisitors = new com.useoptic.diff.shapes.JsonLikeAndSpecDiffVisitors(resolvers, context.spec, emitShapeDiff, markShapeTrailAsVisited)
              val traverser = new com.useoptic.diff.shapes.JsonLikeAndSpecTraverser(resolvers, context.spec, shapeDiffVisitors)
              val body = BodyUtilities.parseBody(interaction.request.body)
              traverser.traverseRootShape(body, expected.shapeId)

              if (diffs.isEmpty) {
                visitedShapeTrails = visitedShapes
              }
              diffs.foreach(diff => {
                val interactionTrail = InteractionTrail(Seq(RequestBody(contentTypeHeader)))
                val requestsTrail = SpecRequestBody(request.requestId)
                emit(UnmatchedRequestBodyShape(interactionTrail, requestsTrail, diff))
              })
            } else {
              visitedWithUnmatchedContentTypes = visitedWithUnmatchedContentTypes + request.requestId
            }
          }
        }
      }
      case None => {
      }
    }
  }


  override def end(interaction: HttpInteraction, context: PathVisitorContext): Unit = {
    if (context.path.isEmpty) {
      return
    }
    if (visitedWithMatchedContentTypes.isEmpty) {

      val actualContentType = ContentTypeHelpers.contentType(interaction.request)
      val interactionTrail = actualContentType match {
        case Some(contentType) => InteractionTrail(Seq(Url(), Method(interaction.request.method), RequestBody(contentType)))
        case None => InteractionTrail(Seq(Url(), Method(interaction.request.method)))
      }
      emit(
        UnmatchedRequestBodyContentType(
          interactionTrail,
          SpecPath(context.path.get))
      )
    }
  }
}

class DiffResponseBodyVisitor(resolvers: ShapesResolvers, emit: (InteractionDiffResult) => Unit) extends ResponseBodyVisitor {
  var visitedWithUnmatchedContentTypes: Set[HttpResponse] = Set()
  var visitedWithMatchedContentTypes: Set[ResponseId] = Set()
  var visitedShapeTrails: Counter[ShapeTrail] = new Counter[ShapeTrail]


  override def begin(): Unit = {
    visitedWithUnmatchedContentTypes = Set()
    visitedWithMatchedContentTypes = Set()
  }

  override def visit(interaction: HttpInteraction, context: ResponseBodyVisitorContext): Unit = {
    if (context.path.isEmpty) {
      return
    }
    if (context.response.isEmpty) {
      return
    }
    val response = context.response.get
    val actualContentType = ContentTypeHelpers.contentType(interaction.response)
    val expectedContentType = response.responseDescriptor.bodyDescriptor
    (expectedContentType, actualContentType) match {
      case (d: UnsetBodyDescriptor, None) => {
        // Logger.log("spec says no body, response has no body")
        visitedWithMatchedContentTypes = visitedWithMatchedContentTypes + response.responseId
      }
      case (d: UnsetBodyDescriptor, Some(contentTypeHeader)) => {
        // Logger.log("spec says no body, response has body")
        visitedWithUnmatchedContentTypes = visitedWithUnmatchedContentTypes + response
      }
      case (d: ShapedBodyDescriptor, None) => {
        // Logger.log("spec says body, response has no body")
        visitedWithUnmatchedContentTypes = visitedWithUnmatchedContentTypes + response
      }
      case (expected: ShapedBodyDescriptor, Some(contentTypeHeader)) => {
        // Logger.log("comparing response bodies")
        if (expected.httpContentType == contentTypeHeader) {
          visitedWithMatchedContentTypes = visitedWithMatchedContentTypes + response.responseId

          val diffs = scala.collection.mutable.ArrayBuffer[ShapeDiffResult]()

          def emitShapeDiff(diff: ShapeDiffResult) = {
            diffs.append(diff)
          }

          val visitedShapes = new Counter[ShapeTrail]

          def markShapeTrailAsVisited(trail: ShapeTrail) = {
            visitedShapes.increment(trail)
          }

          val shapeDiffVisitors = new com.useoptic.diff.shapes.JsonLikeAndSpecDiffVisitors(resolvers, context.spec, emitShapeDiff, markShapeTrailAsVisited)
          val traverser = new com.useoptic.diff.shapes.JsonLikeAndSpecTraverser(resolvers, context.spec, shapeDiffVisitors)
          val body = BodyUtilities.parseBody(interaction.response.body)
          traverser.traverseRootShape(body, expected.shapeId)

          if (diffs.isEmpty) {
            visitedShapeTrails = visitedShapes
          }
          diffs.foreach(diff => {
            val interactionTrail = InteractionTrail(Seq(ResponseBody(contentTypeHeader, interaction.response.statusCode)))
            val requestsTrail = SpecResponseBody(response.responseId)
            emit(UnmatchedResponseBodyShape(interactionTrail, requestsTrail, diff))
          })
        } else {
          visitedWithUnmatchedContentTypes = visitedWithUnmatchedContentTypes + response
        }
      }
    }
  }

  override def end(interaction: HttpInteraction, context: PathVisitorContext): Unit = {
    if (context.path.isEmpty) {
      return
    }
    if (visitedWithMatchedContentTypes.isEmpty) {
      val actualContentType = ContentTypeHelpers.contentType(interaction.response)
      val interactionTrail = actualContentType match {
        case Some(contentType) => InteractionTrail(Seq(Method(interaction.request.method), ResponseBody(contentType, interaction.response.statusCode)))
        case None => InteractionTrail(Seq(Method(interaction.request.method), ResponseStatusCode(interaction.response.statusCode)))
      }
      emit(
        UnmatchedResponseBodyContentType(
          interactionTrail,
          SpecPath(context.path.get))
      )
    }
  }
}

class DiffInteractionVisitor(emit: (InteractionDiffResult) => Unit) extends InteractionVisitor {
  override def begin(): Unit = {}

  override def end(interaction:HttpInteraction, context: PathVisitorContext): Unit = {}
}

class DiffVisitors(resolvers: ShapesResolvers, emit: (InteractionDiffResult) => Unit) extends Visitors {

  override val pathVisitor = new DiffPathVisitor(emit)

  override val requestBodyVisitor = new DiffRequestBodyVisitor(resolvers, emit)

  override val responseBodyVisitor = new DiffResponseBodyVisitor(resolvers, emit)

  override val interactionVisitor = new DiffInteractionVisitor(emit)
}
