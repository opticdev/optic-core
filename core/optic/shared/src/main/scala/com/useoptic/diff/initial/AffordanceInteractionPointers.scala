package com.useoptic.diff.initial

import com.useoptic.types.capture.JsonLike

case class AffordanceInteractionPointers(wasString: Set[String] = Set.empty,
                                         wasNumber: Set[String] = Set.empty,
                                         wasBoolean: Set[String] = Set.empty,
                                         wasNull: Set[String] = Set.empty,
                                         wasArray: Set[String] = Set.empty,
                                         wasObject: Set[String] = Set.empty,
                                         wasMissing: Set[String] = Set.empty
                                        ) {

  private def touchString(interactionId: String) = this.copy(wasString = update(this.wasString, interactionId))
  private def touchNumber(interactionId: String) = this.copy(wasNumber = update(this.wasNumber, interactionId))
  private def touchBoolean(interactionId: String) = this.copy(wasBoolean = update(this.wasBoolean, interactionId))
  private def touchNull(interactionId: String) = this.copy(wasNull = update(this.wasNull, interactionId))
  private def touchArray(interactionId: String) = this.copy(wasArray = update(this.wasArray, interactionId))
  private def touchObject(interactionId: String) = this.copy(wasObject = update(this.wasObject, interactionId))
  private def touchMissing(interactionId: String) = this.copy(wasMissing = update(this.wasMissing, interactionId))

  private def update(set: Set[String], newItem: String): Set[String] = {
    val max = 5
    if (set.size < max) {
      set + newItem
    } else set
  }

  def handleJsonLike(jsonLike: Option[JsonLike], interactionId: String): AffordanceInteractionPointers = {
    var current = this
    if (jsonLike.isDefined) {
      if (jsonLike.get.isString) {
        current = touchString(interactionId)
      }
      if (jsonLike.get.isNumber) {
        current = touchNumber(interactionId)
      }
      if (jsonLike.get.isBoolean) {
        current = touchBoolean(interactionId)
      }
      if (jsonLike.get.isNull) {
        current = touchNull(interactionId)
      }
      if (jsonLike.get.isArray) {
        current = touchArray(interactionId)
      }
      if (jsonLike.get.isObject) {
        current = touchObject(interactionId)
      }
    } else {
      current = touchMissing(interactionId)
    }
    current
  }

}
