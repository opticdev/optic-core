package com.useoptic.diff.interactions.interpreters.copy

import scala.scalajs.js.annotation.JSExportAll

object InterpreterCopyHelper {

  type Copy = Seq[Text]
  @JSExportAll
  case class CopyPair(action: Copy, pastTense: Copy)

  implicit class InterpreterCopyStringHelper(val string: String) {
    def text = Text(string, InterpreterCopyStyles.normal)
    def t = Text(string, InterpreterCopyStyles.normal)
    def code = Text(string, InterpreterCopyStyles.code)
  }

  implicit class InterpreterCopyHelper(val copy: Copy) {
    def flattenToString = {
      copy.map{
        case Text(value, style) if style == InterpreterCopyStyles.normal => value
        case Text(value, style) if style == InterpreterCopyStyles.code => s"'${value}'"
      }.mkString(" ")
    }
  }

}

object InterpreterCopyStyles {
  def normal: String = "normal"
  def code: String = "code"
}

@JSExportAll
case class Text(value: String, style: String)
