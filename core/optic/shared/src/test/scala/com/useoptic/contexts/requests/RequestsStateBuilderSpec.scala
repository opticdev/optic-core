package com.useoptic.contexts.requests

import com.useoptic.contexts.requests.Commands.AddPathComponent
import com.useoptic.contexts.shapes.ShapesService
import com.useoptic.dsa.OpticIds
import org.scalatest.FunSpec

class RequestsStateBuilderSpec extends FunSpec {
  def fixture = new {
    implicit val ids = OpticIds.newDeterministicIdGenerator
    val service = new RequestsService(new ShapesService)
    val aggregateId1 = "a1"

  }

  describe("Initial State") {
    it("should have just a root path") {
      val f = fixture
      import f._
      assert(service.currentState(aggregateId1).pathComponents.size == 1)
    }
  }
  describe("setting up a few nested paths") {
    it("should add a child path component to root") {
      val f = fixture
      import f._
      service.handleCommand(aggregateId1, AddPathComponent("p1", "root", "a"))
      assert(service.currentState(aggregateId1).pathComponents.size == 2)
    }
    it("should fail to add a duplicate id") {
      val f = fixture
      import f._
      service.handleCommand(aggregateId1, AddPathComponent("p1", "root", "a"))
      assertThrows[java.lang.IllegalArgumentException] {
        service.handleCommand(aggregateId1, AddPathComponent("p1", "root", "b"))
      }
    }
    it("should add nested paths") {
      val f = fixture
      import f._
      service.handleCommand(aggregateId1, AddPathComponent("p1", "root", "a"))
      service.handleCommand(aggregateId1, AddPathComponent("p2", "p1", "b"))
      service.handleCommand(aggregateId1, AddPathComponent("p3", "root", "b"))
      assert(service.currentState(aggregateId1).pathComponents.size == 4)
    }
  }
}
