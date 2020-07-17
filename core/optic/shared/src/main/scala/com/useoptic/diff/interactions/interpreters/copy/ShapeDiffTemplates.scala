package com.useoptic.diff.interactions.interpreters.copy

import com.useoptic.diff.interactions.interpreters.copy.InterpreterCopyHelper._

object ShapeDiffTemplates {

  def unspecifiedObserved(description: String): Copy =
    Seq(description.code, "observed".t)

  def missing(description: String): Copy =
    Seq(description.code, "is missing".t)

  def wrongShape(description: String, expectation: String): Copy =
    Seq(description.code, "is not a".t, expectation.code)


  //@todo improve
  def newX(description: Copy): Copy = {
    Seq("New".t) ++ description
  }
  def missingRequired(description: Copy): Copy = {
    Seq("Missing required".t) ++ description
  }

  def expectationNotMet(description: Copy, expectation: String): Copy = {
    Seq("Expected".t) ++ description ++ Seq("to be".t, expectation.code)
  }

}
