package com.useoptic


import com.useoptic.contexts.rfc.Events.RfcEvent
import com.useoptic.ddd.{AggregateId, EventStore}
import com.useoptic.serialization.EventSerialization
import io.circe.Json

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.scalajs.convertJsonToJs

import scala.scalajs.js.UndefOr

@JSExportTopLevel("ScalaJSHelpers")
@JSExportAll
object ScalaJSHelpers {

  import js.JSConverters._

  def toJsArray[A](seq: Seq[A]): js.Array[A] = {
    seq.toJSArray
  }

  def length[A](seq: Seq[A]): Int = {
    seq.length
  }

  def getIndex[A](seq: Seq[A], index: Int): A = seq(index)

  def getOrUndefined[A](option: Option[A]): UndefOr[A] = {
    option.orUndefined
  }

  def toOption[A](undefOr: UndefOr[A]): Option[A] = {
    undefOr.toOption
  }

  def toSome[A](undefOr: UndefOr[A]): Some[A] = {
    require(undefOr.nonEmpty, "toSome requires defined value")
    Some(undefOr.get)
  }

  def headOrUndefined[A](seq: Seq[A]): UndefOr[A] = {
    seq.headOption.orUndefined
  }

  def headOrUndefined[A](set: Set[A]): UndefOr[A] = {
    set.headOption.orUndefined
  }

  def getOrUndefinedJson(option: Option[Json]): UndefOr[js.Any] = {
    option.map(i => convertJsonToJs(i)).orUndefined
  }

  def getJson(j: Json): js.Any = convertJsonToJs(j)

  def eventsJsArray(events: Vector[RfcEvent]): js.Any = {
    convertJsonToJs(EventSerialization.toJson(events))
  }

}
