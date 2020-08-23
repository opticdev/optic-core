package com.useoptic

import com.useoptic.changelog.Changelog
import com.useoptic.contexts.rfc.Events.RfcEvent
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.scalajs.{convertJsToJson, convertJsonToJs}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExport
@JSExportAll
object ChangeLogFacade {

  def from(target: js.Any, head: js.Any): js.Any = {
    val targetEvents = EventSerializationJs.fromJs(target)
    val headEvents = EventSerializationJs.fromJs(head)
    convertJsonToJs(
      Changelog.from(targetEvents, headEvents).asJson
    )
  }
}
