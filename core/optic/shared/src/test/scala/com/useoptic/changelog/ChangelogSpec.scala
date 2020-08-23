package com.useoptic.changelog

import com.useoptic.contexts.rfc.Events.BatchCommitEnded
import com.useoptic.diff.JsonFileFixture
import org.scalatest.FunSpec

class ChangelogSpec extends FunSpec with JsonFileFixture {
  lazy val todoEvents = eventsFromSpec("todos")

  lazy val commitOffsets = todoEvents.zipWithIndex.collect {
    case (i, index) if i.isInstanceOf[BatchCommitEnded] => index
  }

  lazy val emptySpec = todoEvents.take(0)
  lazy val otherSpecs = commitOffsets.map(i => todoEvents.take(i))
  lazy val spec1 = otherSpecs(0)
  lazy val spec2 = otherSpecs(1)
  lazy val spec3 = otherSpecs(2)

  it("finds no diff between two empty specs") {
    assert(Changelog.from(emptySpec, emptySpec).size == 0)
  }

  it("finds 3 additions between an empty and spec with 3 endpoints") {
    val changelog = Changelog.from(emptySpec, spec3)
    assert(changelog.count(_.added) == 3)
  }

  it("finds 3 deletions between with 3 endpoints and an empty spec") {
    val changelog = Changelog.from(spec3, emptySpec)
    assert(changelog.count(_.removed) == 3)
  }

  it("finds 1 addition between with 2 endpoints and 3 endpoints") {
    val changelog = Changelog.from(spec2, spec3)
    assert(changelog.count(_.added) == 1)
    assert(changelog.count(_.removed) == 0)
    assert(changelog.count(_.updated) == 0)
  }


}
