package com.useoptic.diff.interactions.interpreters

import com.useoptic.contexts.rfc.RfcState
import com.useoptic.diff.InteractiveDiffInterpretation
import com.useoptic.diff.initial.ShapeBuildingStrategy
import com.useoptic.diff.interactions.interpretations.BasicInterpretations
import com.useoptic.diff.interactions._
import com.useoptic.diff.interpreters.InteractiveDiffInterpreter
import com.useoptic.diff.shapes.{UnmatchedShape, UnspecifiedShape}
import com.useoptic.dsa.OpticDomainIds
import com.useoptic.types.capture.HttpInteraction

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExport
@JSExportAll
class BasicInterpreters(rfcState: RfcState)(implicit ids: OpticDomainIds) extends InteractiveDiffInterpreter[InteractionDiffResult] {
  implicit val shapeBuildingStrategy = ShapeBuildingStrategy.learnASingleInteraction
  override def interpret(diff: InteractionDiffResult, interactions: Vector[HttpInteraction]): Seq[InteractiveDiffInterpretation] = {
    val interpretations = new BasicInterpretations(rfcState)
    diff match {
      // we handle this in a separate ui so it should not get to the diff ui
      case d: UnmatchedRequestUrl => Seq.empty
      // we handle this in a separate ui so it should not get to the diff ui
      case d: UnmatchedRequestMethod => Seq.empty
      case d: UnmatchedResponseStatusCode => Seq(
        interpretations.AddResponse(d.interactionTrail, d.requestsTrail)
      )
      case d: UnmatchedRequestBodyContentType => Seq(
        interpretations.AddRequestContentType(d.interactionTrail, d.requestsTrail, interactions)
      )
      case d: UnmatchedResponseBodyContentType => Seq(
        interpretations.AddResponseContentType(d.interactionTrail, d.requestsTrail, interactions)
      )
      case _ => Seq.empty
    }
  }

  override def interpret(diff: InteractionDiffResult, interaction: HttpInteraction): Seq[InteractiveDiffInterpretation] = {
    Seq.empty
  }
}
