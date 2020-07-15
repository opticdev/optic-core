package com.useoptic.end_to_end

import com.useoptic.contexts.rfc.{RfcService, RfcServiceJSFacade}
import com.useoptic.diff.JsonFileFixture
import com.useoptic.dsa.OpticIds
import com.useoptic.end_to_end.fixtures.JsonExamples
import com.useoptic.end_to_end.snapshot_task.{ShapeNameTask}
import com.useoptic.ux.ShapeNameRenderer
import org.scalatest.FunSpec

class ShapeNamesSpec extends ShapeNameTask with JsonFileFixture {

  lazy val todoEvents = eventsFrom("todo")

  when("a string primitive", () => ShapeNameTask.Input(
    todoEvents, "WDPBZw_9"
  ))

  when("an object with fields", () => ShapeNameTask.Input(
    todoEvents, "gKfXSj_0"
  ))

  whenOnly("a list of objects", () => ShapeNameTask.Input(
    todoEvents, "WDPBZw_0"
  ))

  runSuite
}
