package com.useoptic.diff

import com.useoptic.contexts.rfc.Commands.RfcCommand
import com.useoptic.contexts.shapes.projections.TrailTags
import com.useoptic.diff.ChangeType.ChangeType
import com.useoptic.diff.interactions.interpreters.copy.InterpreterCopyHelper.CopyPair
import com.useoptic.diff.shapes.JsonTrail

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExportAll
case class InteractiveDiffInterpretation(
                                          //communication
                                          copyPair: CopyPair,
                                          //domain
                                          commands: Seq[RfcCommand],
                                          //visualizations
                                          changeType: ChangeType) {
  def changeTypeAsString: String = changeType.toString

  import com.useoptic.diff.interactions.interpreters.copy.InterpreterCopyHelper._ //help migrate
  def action: String = copyPair.action.flattenToString
  def pastTenseAction: String = copyPair.pastTense.flattenToString
}

object ChangeType extends Enumeration {
  type ChangeType = Value
  val Addition, Removal, Update = Value
}
