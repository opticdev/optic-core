package com.useoptic.ux

import com.useoptic.DiffStats
import com.useoptic.contexts.requests.Commands.{PathComponentId, RequestMethod}
import com.useoptic.contexts.requests.Utilities
import com.useoptic.contexts.rfc.RfcState
import com.useoptic.diff.{ChangeType, DiffResult, InteractiveDiffInterpretation}
import com.useoptic.diff.helpers.DiffHelpers
import com.useoptic.diff.initial.ShapeBuildingStrategy
import com.useoptic.diff.interactions.interpreters.{DefaultInterpreters, DiffDescriptionInterpreters}
import com.useoptic.diff.interactions.{BodyUtilities, InteractionDiffResult, InteractionTrail, RequestSpecTrail, RequestSpecTrailHelpers, Resolvers, SpecPath, SpecRequestBody, SpecRequestRoot, SpecResponseBody, SpecResponseRoot, SpecRoot, UnmatchedRequestBodyContentType, UnmatchedRequestBodyShape, UnmatchedRequestMethod, UnmatchedRequestUrl, UnmatchedResponseBodyContentType, UnmatchedResponseBodyShape, UnmatchedResponseStatusCode}
import com.useoptic.diff.shapes.ShapeDiffResult
import com.useoptic.diff.shapes.resolvers.ShapesResolvers
import com.useoptic.dsa.OpticIds
import com.useoptic.logging.Logger
import com.useoptic.types.capture.HttpInteraction

import scala.scalajs.js.annotation.JSExportAll
import scala.util.Try

@JSExportAll
class DiffManager(initialInteractions: Seq[HttpInteraction], onUpdated: () => Unit = () => {}) {

  private var _currentRfcState: RfcState = null
  private var _resolvers: ShapesResolvers = null
  private var _endpointFilter: Option[(PathComponentId, RequestMethod)] = None
  private var diffPreviewer: DiffPreviewer = null
  private var _interactionsGroupedByDiffs: DiffsToInteractionsMap = Map.empty
  private var _interactions: Seq[HttpInteraction] = initialInteractions

  //put simulated rfc state here.
  def updatedRfcState(rfcState: RfcState, resolvers: ShapesResolvers): Unit = {
    if (_currentRfcState != rfcState) {
      _currentRfcState = rfcState

      _resolvers = resolvers
      diffPreviewer = new DiffPreviewer(_resolvers, _currentRfcState)
      recomputeDiff
    }
  }

  def updateInteractions(httpInteractions: Seq[HttpInteraction]): Unit = {
    if (_interactions != httpInteractions) {
      _interactions = httpInteractions
      recomputeDiff
    }
  }

  def updateEndpointFilter(pathId: PathComponentId, method: RequestMethod, skipRecompute: Boolean = false): Unit = {
    val newFilter = Some((pathId, method))
    val shouldRecompute = newFilter != _endpointFilter
    _endpointFilter = newFilter
    if (shouldRecompute && !skipRecompute) {
      recomputeDiff()
    }
  }

  def clearEndpointFilter(): Unit = {
    val newFilter = None
    val shouldRecompute = newFilter != _endpointFilter
    _endpointFilter = newFilter
    if (shouldRecompute) {
      recomputeDiff()
    }
  }

  def recomputeDiff() = {
    if (_currentRfcState != null) {

      val interactionsFilter: HttpInteraction => Boolean = if (_endpointFilter.isDefined) { interaction =>
        val (pathId, method) = _endpointFilter.get
        method == interaction.request.method &&
        Utilities.resolvePath(interaction.request.path, _currentRfcState.requestsState.pathComponents).contains(pathId)
      }  else _ => true

      // Never learn from the request body for an interaction that yields a non 2xx-response
      def filterInteractionsWithErrorResponses(diff: InteractionDiffResult, interactions: Seq[HttpInteraction]) = diff match {
        case _: UnmatchedRequestBodyContentType => interactions.filter(i => i.response.statusCode >= 200 && i.response.statusCode < 400)
        case _: UnmatchedRequestBodyShape => interactions.filter(i => i.response.statusCode >= 200 && i.response.statusCode < 400)
        case _ => interactions
      }

      val filtered = _interactions.filter(interactionsFilter)

      println(s"Recomputing diffs ${filtered.length} / ${_interactions.length}")

      _interactionsGroupedByDiffs = DiffHelpers.groupByDiffs(_resolvers, _currentRfcState, filtered).collect {
        case (diff, interactions) => (diff, filterInteractionsWithErrorResponses(diff, interactions))
      }.filter(_._2.nonEmpty)


    } else {
      _interactionsGroupedByDiffs = Map.empty
    }
    onUpdated()
  }

  def filterIgnored(ignoredDiffs: Seq[DiffResult]): Map[InteractionDiffResult, Seq[HttpInteraction]] = {
    val ignoredKeys = _interactionsGroupedByDiffs.keySet intersect ignoredDiffs.toSet.asInstanceOf[Set[InteractionDiffResult]]
    _interactionsGroupedByDiffs.filterKeys(i => !ignoredKeys.contains(i))
  }

  def unmatchedUrls(alphabetize: Boolean = false, ignoredDiffs: Seq[DiffResult] = Seq.empty): Seq[UndocumentedURL] = {
    if (_currentRfcState == null) {
      return Seq.empty
    }

    def checkForEmptyContentType(interactionTrail: InteractionTrail, specTrail: RequestSpecTrail, interactions: Seq[HttpInteraction]) = {
      Logger.log(specTrail)
      val pathOption = RequestSpecTrailHelpers.pathId(specTrail)
      if (pathOption.isDefined) {
        interactions.flatMap(interaction => {
          val noOps = Resolvers.resolveOperations(interaction.request.method, pathOption.get, _currentRfcState.requestsState).isEmpty
          val noResponses = Resolvers.resolveResponses(interaction.request.method, pathOption.get, _currentRfcState.requestsState).isEmpty

          if (noOps && noResponses) {
            Some((interaction.request.method, interaction.request.path, pathOption) -> interaction)
          } else {
            None
          }
        })
      } else {
        Seq.empty
      }
    }

    val fromDiff = filterIgnored(ignoredDiffs).collect {
      case (UnmatchedRequestUrl(interactionTrail, requestsTrail), interactions) => {
        interactions.map(i => (i.request.method, i.request.path, None) -> i)
      }
      case (UnmatchedRequestBodyContentType(interactionTrail, specTrail), interactions) => {
        checkForEmptyContentType(interactionTrail, specTrail, interactions)
      }
      case (UnmatchedResponseBodyContentType(interactionTrail, specTrail), interactions) => {
        checkForEmptyContentType(interactionTrail, specTrail, interactions)
      }
      case (UnmatchedResponseBodyContentType(interactionTrail, specTrail), interactions) => {
        checkForEmptyContentType(interactionTrail, specTrail, interactions)
      }
      case (UnmatchedResponseStatusCode(interactionTrail, specTrail), interactions) => {
        checkForEmptyContentType(interactionTrail, specTrail, interactions)
      }
    }.flatten
      .groupBy(_._1)
      .mapValues(i => i.map(_._2))
      .filter(_._2.exists(i => i.response.statusCode >= 200 && i.response.statusCode < 400))

    val allUnmatchedUrls = fromDiff.map { case ((method, path, pathOption), interactions) => UndocumentedURL(method, path, pathOption, interactions.toSeq) }.toSeq

    if (alphabetize) {
      allUnmatchedUrls.sortBy(_.path)
    } else {
      allUnmatchedUrls.sortBy(_.interactions.size).reverse
    }
  }

  def allUnmatchedPaths: Seq[String] = unmatchedUrls(true, Seq.empty).map(_.path).distinct

  def endpointDiffs(ignoredDiffs: Seq[DiffResult], filterUnmatched: Boolean = false): Seq[EndpointDiff] = {
    val diffs = filterIgnored(ignoredDiffs)

    val allEndpointDiffs = diffs.collect {
      case (_: UnmatchedRequestUrl, interactions) => None
      case (d: InteractionDiffResult, interactions) => {
        d.requestsTrail match {
          case SpecRoot() => None
          case SpecPath(pathId) => Some(pathId, interactions.head.request.method, d)
          case SpecRequestRoot(requestId) => {
            val request = _currentRfcState.requestsState.requests(requestId).requestDescriptor
            Some(request.pathComponentId, request.httpMethod, d)
          }
          case SpecRequestBody(requestId) => {
            val request = _currentRfcState.requestsState.requests(requestId).requestDescriptor
            Some(request.pathComponentId, request.httpMethod, d)
          }
          case SpecResponseRoot(responseId) => {
            val response = _currentRfcState.requestsState.responses(responseId).responseDescriptor
            Some(response.pathId, response.httpMethod, d)
          }
          case SpecResponseBody(responseId) => {
            val response = _currentRfcState.requestsState.responses(responseId).responseDescriptor
            Some(response.pathId, response.httpMethod, d)
          }
        }
      }
      case _ => None
    }.flatten

    val descriptionInterpreters = new DiffDescriptionInterpreters(_currentRfcState)(ids = OpticIds.generator)

    val endpointDiffs = allEndpointDiffs.groupBy(i => (i._1, i._2)).map {
      case ((path, method), v) => {
        val diffs = v.map(_._3).toSet
        val changeTypes = diffs.map(diff => descriptionInterpreters.interpret(diff, _interactionsGroupedByDiffs(diff).head).changeType)
        EndpointDiff(method, path,
          changeTypes.count(_ == ChangeType.Addition),
          changeTypes.count(_ == ChangeType.Update),
          changeTypes.count(_ == ChangeType.Removal)
        )
      }
    }.toSeq

    val unmatched = unmatchedUrls(true, ignoredDiffs)

    //don't show an endpoint diff if its in the unmatched list
    if (filterUnmatched) endpointDiffs.filterNot(i => unmatched.exists(u => u.pathId.contains(i.pathId) && u.method == i.method)) else endpointDiffs
  }

  def stats(ignoredDiffs: Seq[DiffResult]): DiffStats = {
    DiffStats(
      _interactions.size,
      _interactionsGroupedByDiffs.keys.filterNot {
        case UnmatchedRequestUrl(_, _) => true
        case _ => false
      }.size,
      unmatchedUrls(true, ignoredDiffs).size
    )
  }

  def managerForPathAndMethod(pathComponentId: PathComponentId, httpMethod: String, ignoredDiffs: Seq[DiffResult]): PathAndMethodDiffManager = {
    val parentManagerUpdate = (rfcState: RfcState, resolvers: ShapesResolvers) => updatedRfcState(rfcState, resolvers)


    val filterThisEndpoint = {
      //collect all request and response ids we have diffs computed for
      val requestIds = _currentRfcState.requestsState.requests.collect {
        case req if req._2.requestDescriptor.httpMethod == httpMethod && req._2.requestDescriptor.pathComponentId == pathComponentId => req._1
      }.toSet

      val responseIds = _currentRfcState.requestsState.responses.collect {
        case res if res._2.responseDescriptor.httpMethod == httpMethod && res._2.responseDescriptor.pathId == pathComponentId => res._1
      }.toSet
      val ignoredDiffsSet = ignoredDiffs.toSet.asInstanceOf[Set[InteractionDiffResult]]
      _interactionsGroupedByDiffs.filterKeys(key => {
        !ignoredDiffsSet.contains(key) &&
          (key match {
            case _: UnmatchedRequestUrl => false
            case d: InteractionDiffResult => {
              d.requestsTrail match {
                case SpecRoot() => false
                case SpecPath(pathId) => pathComponentId == pathId && d.interactionTrail.httpMethod().contains(httpMethod)
                case SpecRequestRoot(requestId) => requestIds.contains(requestId)
                case SpecRequestBody(requestId) => requestIds.contains(requestId)
                case SpecResponseRoot(responseId) => responseIds.contains(responseId)
                case SpecResponseBody(responseId) => responseIds.contains(responseId)
              }
            }
            case _ => false
          })
      })
    }

    import com.useoptic.utilities.DistinctBy._
    val withGrouping = filterThisEndpoint.keys.toVector.distinctByIfDefined(i => (i.shapeDiffResultOption.map(a => (a.shapeTrail, a.getClass.getSimpleName))))
    val filteredByGroupings = filterThisEndpoint.filterKeys(i => withGrouping.contains(i))

    val ungroupedShapeDiffs = filterThisEndpoint.keySet.collect { case i if i.shapeDiffResultOption.isDefined => i.shapeDiffResultOption.get }


    new PathAndMethodDiffManager(pathComponentId, httpMethod)(filteredByGroupings, ungroupedShapeDiffs, _currentRfcState, _resolvers, diffPreviewer) {
      def updatedRfcState(rfcState: RfcState, resolvers: ShapesResolvers): Unit = parentManagerUpdate(rfcState, resolvers)
    }
  }
}

@JSExportAll
abstract class PathAndMethodDiffManager(pathComponentId: PathComponentId, httpMethod: String)(implicit val interactionsGroupedByDiffs: DiffsToInteractionsMap, ungroupedShapeDiffs: Set[ShapeDiffResult], rfcState: RfcState, resolvers: ShapesResolvers, diffPreviewer: DiffPreviewer) {

  implicit val shapeBuildingStrategy = ShapeBuildingStrategy.inferPolymorphism

  def updatedRfcState(rfcState: RfcState, resolvers: ShapesResolvers): Unit

  def suggestionsForDiff(diff: InteractionDiffResult, interactions: Vector[HttpInteraction]): Seq[InteractiveDiffInterpretation] = {
    val basicInterpreter = new DefaultInterpreters(resolvers, rfcState)(ids = OpticIds.generator)

    basicInterpreter.interpret(diff, interactions)
  }

  def suggestionsForDiff(diff: InteractionDiffResult): Seq[InteractiveDiffInterpretation] = suggestionsForDiff(diff, interactionsGroupedByDiffs(diff.asInstanceOf[InteractionDiffResult]).toVector)

  def noDiff = interactionsGroupedByDiffs.keySet.isEmpty

  def diffCount: Int = interactionsGroupedByDiffs.keys.size

  def interactionsWithDiffsCount: Int = interactionsGroupedByDiffs.values.flatten.size

  def diffRegions: TopLevelRegions = {

    val descriptionInterpreters = new DiffDescriptionInterpreters(rfcState)(ids = OpticIds.generator)

    def toNewRegionSuggestion(inferPolymorphism: Boolean, diff: InteractionDiffResult, interactions: Vector[HttpInteraction]): InteractiveDiffInterpretation = {
      if (inferPolymorphism) {
        suggestionsForDiff(diff, interactions)
      } else {
        suggestionsForDiff(diff, Vector(interactions.head))
      }
    }.head

    val newRegions = interactionsGroupedByDiffs.collect {
      case (diff: UnmatchedRequestBodyContentType, interactions) => {
        val description = descriptionInterpreters.interpret(diff, interactions.head)

        val previewRender = (interaction: HttpInteraction) => diffPreviewer.previewBody(interaction.request.body)


        val previewShape = (interaction: HttpInteraction, inferPolymorphism: Boolean) => {
          if (inferPolymorphism) {
            val bodies = interactions.map(_.request.body).flatMap(BodyUtilities.parseBody).toVector
            val preview = diffPreviewer.shapeOnlyFromShapeBuilder(bodies)
            preview.map(_._2)
          } else {
            diffPreviewer.shapeOnlyFromShapeBuilder(Vector(BodyUtilities.parseBody(interaction.request.body)).flatten).map(_._2)
          }
        }

        NewRegionDiffBlock(diff, interactions, inRequest = true, inResponse = false, diff.interactionTrail.requestBodyContentTypeOption(), None, description)(
          (inferPolymorphism: Boolean) => toNewRegionSuggestion(inferPolymorphism, diff, interactions.toVector),
          previewRender,
          previewShape
        )
      }
      case (diff: UnmatchedResponseBodyContentType, interactions) => {
        val description = descriptionInterpreters.interpret(diff, interactions.head)

        val previewRender = (interaction: HttpInteraction) => diffPreviewer.previewBody(interaction.response.body)


        val previewShape = (interaction: HttpInteraction, inferPolymorphism: Boolean) => {
          if (inferPolymorphism) {
            val bodies = interactions.map(_.response.body).flatMap(BodyUtilities.parseBody).toVector
            val preview = diffPreviewer.shapeOnlyFromShapeBuilder(bodies)
            preview.map(_._2)
          } else {
            diffPreviewer.shapeOnlyFromShapeBuilder(Vector(BodyUtilities.parseBody(interaction.response.body)).flatten).map(_._2)
          }
        }

        NewRegionDiffBlock(diff, interactions, inRequest = false, inResponse = true, diff.interactionTrail.responseBodyContentTypeOption(), Some(diff.interactionTrail.statusCode()), description)(
          (inferPolymorphism: Boolean) => toNewRegionSuggestion(inferPolymorphism, diff, interactions.toVector),
          previewRender,
          previewShape
        )
      }
      case (diff: UnmatchedResponseStatusCode, interactions) => {
        val description = descriptionInterpreters.interpret(diff, interactions.head)

        val previewRender = (interaction: HttpInteraction) => diffPreviewer.previewBody(interaction.response.body)

        val previewShape = (interaction: HttpInteraction, inferPolymorphism: Boolean) => {
          if (inferPolymorphism) {

            val bodies = interactions.map(_.response.body).flatMap(BodyUtilities.parseBody).toVector
            val preview = diffPreviewer.shapeOnlyFromShapeBuilder(bodies)
            preview.map(_._2)
          } else {
            diffPreviewer.shapeOnlyFromShapeBuilder(Vector(BodyUtilities.parseBody(interaction.response.body)).flatten).map(_._2)
          }
        }

        NewRegionDiffBlock(diff, interactions, inRequest = false, inResponse = true, None, Some(diff.interactionTrail.statusCode()), description)(
          (inferPolymorphism: Boolean) => toNewRegionSuggestion(inferPolymorphism, diff, interactions.toVector),
          previewRender,
          previewShape
        )
      }
    }.toSeq

    val requestShapeRegions = interactionsGroupedByDiffs.filter {
      case (a: UnmatchedRequestBodyShape, _) => true
      case _ => false
    }.toSeq
      .groupBy(_._1.interactionTrail.requestBodyContentTypeOption())
      .flatMap { case (contentType, diffMap) => diffMap.map(i => {
        val (diff, interactions) = i
        val description = descriptionInterpreters.interpret(diff, interactions.head)

        val previewRender = (interaction: HttpInteraction, withRfcState: Option[RfcState]) => {
          val innerRfcState = withRfcState.getOrElse(rfcState)
          val simulatedDiffPreviewer = new DiffPreviewer(innerRfcState)
          simulatedDiffPreviewer.previewDiff(BodyUtilities.parseBody(interaction.request.body), Some(diff.shapeDiffResultOption.get.shapeTrail.rootShapeId), Set(diff.shapeDiffResultOption).flatten, ungroupedShapeDiffs)
        }

        val responseRender = (interaction: HttpInteraction, withRfcState: Option[RfcState]) => Try {
          val innerRfcState = withRfcState.getOrElse(rfcState)
          val simulatedDiffPreviewer = new DiffPreviewer(innerRfcState)
          val responseShapeID = Resolvers.resolveRequestShapeByInteraction(interaction, pathComponentId, innerRfcState.requestsState).get
          simulatedDiffPreviewer.previewDiff(BodyUtilities.parseBody(interaction.response.body), Some(responseShapeID), Set.empty, Set.empty)
        }.toOption.flatten

        BodyShapeDiffBlock(
          diff,
          Seq("Request Body", contentType.get),
          diff.shapeDiffResultOption.get,
          interactions,
          inRequest = true,
          inResponse = false,
          description)(
          () => suggestionsForDiff(diff),
          (interaction: HttpInteraction, withRfcState: Option[RfcState]) => previewRender(interaction, withRfcState).get,
          (interaction: HttpInteraction, withRfcState: Option[RfcState]) => previewRender(interaction, withRfcState),
          (interaction: HttpInteraction, withRfcState: Option[RfcState]) => responseRender(interaction, withRfcState)
        )
      })
      }.toSeq


    val responseShapeRegions = interactionsGroupedByDiffs.filter {
      case (_: UnmatchedResponseBodyShape, _) => true
      case _ => false
    }.toSeq
      .groupBy(_._1.interactionTrail.responseBodyContentTypeOption())
      .flatMap { case (contentType, diffMap) => diffMap.map(i => {
        val (diff, interactions) = i
        val description = descriptionInterpreters.interpret(diff, interactions.head)

        val previewRender = (interaction: HttpInteraction, withRfcState: Option[RfcState]) => {
          val innerRfcState = withRfcState.getOrElse(rfcState)
          val simulatedDiffPreviewer = new DiffPreviewer(innerRfcState)
          simulatedDiffPreviewer.previewDiff(BodyUtilities.parseBody(interaction.response.body), Some(diff.shapeDiffResultOption.get.shapeTrail.rootShapeId), Set(diff.shapeDiffResultOption).flatten, ungroupedShapeDiffs)
        }
        val requestRender = (interaction: HttpInteraction, withRfcState: Option[RfcState]) => Try {
          val innerRfcState = withRfcState.getOrElse(rfcState)
          val simulatedDiffPreviewer = new DiffPreviewer(innerRfcState)
          val requestBodyShapeId = Resolvers.resolveRequestShapeByInteraction(interaction, pathComponentId, innerRfcState.requestsState).get
          simulatedDiffPreviewer.previewDiff(BodyUtilities.parseBody(interaction.request.body), Some(requestBodyShapeId), Set.empty, Set.empty)
        }.toOption.flatten

        BodyShapeDiffBlock(
          diff,
          Seq(s"${interactions.head.response.statusCode} Response", "Body", contentType.get),
          diff.shapeDiffResultOption.get,
          interactions,
          inRequest = false,
          inResponse = true,
          description)(
          () => suggestionsForDiff(diff),
          (interaction: HttpInteraction, withRfcState: Option[RfcState]) => previewRender(interaction, withRfcState).get,
          (interaction: HttpInteraction, withRfcState: Option[RfcState]) => requestRender(interaction, withRfcState),
          (interaction: HttpInteraction, withRfcState: Option[RfcState]) => previewRender(interaction, withRfcState)
        )
      })
      }.toSeq

    TopLevelRegions(newRegions, requestShapeRegions ++ responseShapeRegions)
  }


  def inputStats = {
    s"${interactionsGroupedByDiffs.values.flatten.seq.size} interactions, yielding ${interactionsGroupedByDiffs.keys.size} diffs \n\n ${interactionsGroupedByDiffs.keys.toString()}"
  }
}

