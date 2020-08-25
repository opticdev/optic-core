package com.useoptic.changelog

import better.files.File.Events
import com.useoptic.contexts.rfc.Events.RfcEvent
import com.useoptic.contexts.rfc.projections.{OASDomain, OASProjection}
import com.useoptic.contexts.rfc.{InMemoryQueries, RfcService, RfcServiceJSFacade, RfcState}
import com.useoptic.dsa.OpticIds

object Changelog {

  def from(target: Vector[RfcEvent], head: Vector[RfcEvent]): Vector[ChangelogByEndpoints] = {

    val targetSpec = eventsToOasProjectSpec(target)
    val headSpec = eventsToOasProjectSpec(head)

    val targetEndpoints = endpointHelper(targetSpec)
    val targetEndpointsBare = targetEndpoints.map(_.bare)
    val headEndpoints = endpointHelper(headSpec)
    val headEndpointsBare = headEndpoints.map(_.bare)

    val removed = targetEndpointsBare diff headEndpointsBare
    val added =  headEndpointsBare diff targetEndpointsBare
    val same  = headEndpointsBare intersect targetEndpointsBare

    val sameEndpointsDidChange = same.map(i => {
      val didChange = targetEndpoints.find(_.bare == i).get.lockString !=
      headEndpoints.find(_.bare == i).get.lockString
      i -> didChange
    }).toMap

    val union = same ++ removed ++ added

    union.map(endpoint => {
      val fullEndpoint = Vector(headEndpoints.find(_.bare == endpoint).map(_.operation), targetEndpoints.find(_.bare == endpoint).map(_.operation)).flatten.head
      ChangelogByEndpoints(fullEndpoint.pathId, endpoint.method, endpoint.path, fullEndpoint.summary.getOrElse(s"${endpoint.method} ${endpoint.path}"),
        added.contains(endpoint),
        removed.contains(endpoint),
        sameEndpointsDidChange.find(_._1 == endpoint).map(_._2).getOrElse(false),
      )
    }).toVector.sortBy(i => i.pathId + i.method)
  }

  def eventsToOasProjectSpec(events: Vector[RfcEvent]): OASProjection = {
    val eventStore = RfcServiceJSFacade.makeEventStore()
    val rfcId = "id"
    eventStore.append(rfcId, events)
    implicit val ids = OpticIds.newDeterministicIdGenerator
    val rfcService = new RfcService(eventStore)
    rfcService.currentState(rfcId)

    val queries = new InMemoryQueries(eventStore, rfcService, "id")

    new OASProjection(queries, rfcService, rfcId, "")
  }

  def endpointHelper(oas: OASProjection): Set[EndpointHelper] = {
    oas.oasOperations.flatMap(path => {
      path.operations.map(operation => {

        val requestString = operation._2.requestBody.flatMap(_.asJsonSchema).map(_.noSpaces).getOrElse("no_request_body")
        val responseString = operation._2.responses.toVector.sortBy(_._1).map(i => i._1 + i._2.responseBody
          .flatMap(_.asJsonSchema).map(_.noSpaces)).mkString("\n")

        EndpointHelper(path.absolutePath, operation._1, requestString + responseString, operation._2)
      })
    })
  }

}

// Helpers
case class EndpointHelper(path: String, method: String, lockString: String, operation: OASDomain.Operation) {
  def bare = BareEndpointHelper(path, method)
}
case class BareEndpointHelper(path: String, method: String)

// Results
case class ChangelogByEndpoints(pathId: String,
                                method: String,
                                absolutePath: String,
                                endpointName: String,
                                added: Boolean,
                                removed: Boolean,
                                updated: Boolean)
