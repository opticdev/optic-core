package com.useoptic.diff.interactions.interpreters.copy

object InterpreterCopyHelper {

  type Copy = Seq[InterpreterCopy]
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

sealed trait InterpreterCopy {
  def value: String
}
object InterpreterCopyStyles {
  def normal: String = "normal"
  def code: String = "code"
}

case class Text(value: String, style: String) extends InterpreterCopy
