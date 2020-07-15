package com.useoptic.diff.interactions.interpreters.copy

object InterpreterCopyHelper {

  implicit class InterpreterCopyHelper(string: String) {
    def text = Text(string, InterpreterCopyStyles.normal)
    def code = Text(string, InterpreterCopyStyles.code)
  }

}

sealed trait InterpreterCopy
object InterpreterCopyStyles {
  def normal: String = "normal"
  def code: String = "code"
}

case class Text(value: String, style: String) extends InterpreterCopy
