package com.useoptic.diff

import io.circe.{Json, ParsingFailure, parser}
import better.files.File
import com.useoptic.contexts.rfc.Events.RfcEvent
import com.useoptic.contexts.rfc.{Commands, Events, RfcService, RfcServiceJSFacade, RfcState}
import com.useoptic.ddd.{AggregateId, EventStore}
import com.useoptic.dsa.OpticIds
import com.useoptic.serialization.{CommandSerialization, EventSerialization, InteractionSerialization}
import com.useoptic.types.capture.HttpInteraction

trait JsonFileFixture {

  def parseFile(filePath: String): Either[ParsingFailure, Json] = {
    parser.parse(File(filePath).contentAsString)
  }

  def fromFile(slug: String): Json = {
    val filePath = "optic/shared/src/test/resources/diff-scenarios/" + slug + ".json"

    val attempt = parseFile(filePath)
    if (attempt.isLeft) {
      throw new Error(attempt.left.get)
    }
    attempt.right.get
  }

  def commandsFrom(slug: String): Vector[Commands.RfcCommand] = {
    val filePath = "optic/shared/src/test/resources/diff-scenarios/" + slug + ".commands.json"
    val json = parseFile(filePath).right.get
    CommandSerialization.fromJson(json).get
  }

  def eventsFrom(slug: String): Vector[Events.RfcEvent] = {
    val filePath = "optic/shared/src/test/resources/diff-scenarios/" + slug + ".events.json"
    val json =parseFile(filePath).right.get
    EventSerialization.fromJson(json).get
  }

  // THIS WILL NO LONGER WORK SINCE WE FACTORED CORE OUT
  def universeFromExampleSession(slug: String): Universe = {
    import better.files._
    val filePath = ("../workspaces/ui/public/example-sessions/"+slug+".json")
    val json = parseFile(filePath).right.get
    eventsAndInteractionsFrom(json)
  }

  // THIS WILL NO LONGER WORK SINCE WE FACTORED CORE OUT
  def universeFromUserSession(slug: String): Universe = {
    val filePath ="optic/shared/src/test/scala/com/useoptic/debug_user_issues/" + slug + ".json"

    val json = parseFile(filePath).right.get
    eventsAndInteractionsFrom(json)
  }

  def universeFromFilePath(path: String): Universe = {
    import better.files._
    val json = parseFile(path).right.get
    eventsAndInteractionsFrom(json)
  }

  case class Universe(rfcService: RfcService, rfcId: AggregateId, eventStore: EventStore[RfcEvent], interactions: Vector[HttpInteraction]) {
    def rfcState = rfcService.currentState(rfcId)
  }

  def eventsAndInteractionsFrom(slug: String): Universe = {
    val json = fromFile(slug)
    eventsAndInteractionsFrom(json)
  }

  def eventsAndInteractionsFrom(json: Json): Universe = {
    val jsonInteractions = json.asObject.get.apply("session").get.asObject.get.apply("samples").get.asArray.get
    val interactions = jsonInteractions.map(x => InteractionSerialization.fromJson(x))

    val jsonEvents = json.asObject.get.apply("events").get

    val events = EventSerialization.fromJson(jsonEvents).get
    val eventStore = RfcServiceJSFacade.makeEventStore()
    val rfcId = "testRfcId"
    eventStore.append(rfcId, events)
    implicit val ids = OpticIds.newDeterministicIdGenerator
    val rfcService = new RfcService(eventStore)
    val rfcState = rfcService.currentState(rfcId)
    Universe(rfcService, rfcId, eventStore, interactions)
  }

}
