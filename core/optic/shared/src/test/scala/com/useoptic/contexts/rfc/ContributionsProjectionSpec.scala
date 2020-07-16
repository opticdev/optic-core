package com.useoptic.contexts.rfc

import com.useoptic.contexts.rfc.Commands.AddContribution
import com.useoptic.contexts.rfc.Events.ContributionAdded
import com.useoptic.contexts.rfc.projections.ContributionsProjection
import org.scalatest.FunSpec

class ContributionsProjectionSpec extends FunSpec {

  it("can build projection from events") {

    val projection = ContributionsProjection.fromEvents(
      Vector(
        ContributionAdded("id1", "description", "VALUE A"),
        ContributionAdded("id1", "description", "123"),
        ContributionAdded("id2", "description", "VALUE B"),
        ContributionAdded("id3", "description", "VALUE C"),
      )
    )

    assert(projection == Map(
      "id1" -> Map("description" -> "123"), // last set value
      "id2" -> Map("description" -> "VALUE B"),
      "id3" -> Map("description" -> "VALUE C"),
    ))

  }

}
