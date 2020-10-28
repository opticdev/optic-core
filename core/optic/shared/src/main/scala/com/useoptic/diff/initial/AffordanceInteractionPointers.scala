package com.useoptic.diff.initial

import com.useoptic.diff.shapes.JsonTrail
import com.useoptic.types.capture.JsonLike

case class AffordanceInteractionPointers(wasString: Set[String] = Set.empty,
                                         wasNumber: Set[String] = Set.empty,
                                         wasBoolean: Set[String] = Set.empty,
                                         wasNull: Set[String] = Set.empty,
                                         wasArray: Set[String] = Set.empty,
                                         wasObject: Set[String] = Set.empty,
                                         wasMissing: Set[String] = Set.empty,
                                         //store trails
                                         wasStringTrails: Set[JsonTrail] = Set.empty,
                                         wasNumberTrails: Set[JsonTrail] = Set.empty,
                                         wasBooleanTrails: Set[JsonTrail] = Set.empty,
                                         wasNullTrails: Set[JsonTrail] = Set.empty,
                                         wasArrayTrails: Set[JsonTrail] = Set.empty,
                                         wasObjectTrails: Set[JsonTrail] = Set.empty,
                                         wasMissingTrails: Set[JsonTrail] = Set.empty
                                        ) {

  private def touchString(interactionId: String, jsonTrails: Vector[JsonTrail]) =
    this.copy(wasString = update(this.wasString, interactionId), wasStringTrails = updateTrails(this.wasStringTrails, jsonTrails))
  private def touchNumber(interactionId: String, jsonTrails: Vector[JsonTrail]) =
    this.copy(wasNumber = update(this.wasNumber, interactionId), wasNumberTrails = updateTrails(this.wasNumberTrails, jsonTrails))
  private def touchBoolean(interactionId: String, jsonTrails: Vector[JsonTrail]) =
    this.copy(wasBoolean = update(this.wasBoolean, interactionId), wasBooleanTrails = updateTrails(this.wasBooleanTrails, jsonTrails))
  private def touchNull(interactionId: String, jsonTrails: Vector[JsonTrail]) =
    this.copy(wasNull = update(this.wasNull, interactionId), wasNullTrails = updateTrails(this.wasNullTrails, jsonTrails))
  private def touchArray(interactionId: String, jsonTrails: Vector[JsonTrail]) =
    this.copy(wasArray = update(this.wasArray, interactionId), wasArrayTrails = updateTrails(this.wasArrayTrails, jsonTrails))
  private def touchObject(interactionId: String, jsonTrails: Vector[JsonTrail]) =
    this.copy(wasObject = update(this.wasObject, interactionId), wasObjectTrails = updateTrails(this.wasObjectTrails, jsonTrails))
  private def touchMissing(interactionId: String, jsonTrails: Vector[JsonTrail]) =
    this.copy(wasMissing = update(this.wasMissing, interactionId), wasMissingTrails = updateTrails(this.wasMissingTrails, jsonTrails))

  private def update(set: Set[String], newItem: String): Set[String] = {
    val max = 5
    if (set.size < max) {
      set + newItem
    } else set
  }
  private def updateTrails(set: Set[JsonTrail], newItem: Vector[JsonTrail]): Set[JsonTrail] = {
    (set ++ newItem).take(20)
  }

  def handleJsonLike(jsonLike: Option[JsonLike], normalizedTrailVisitor: DenormalizedTrailCollector, interactionId: String): AffordanceInteractionPointers = {
    var current = this
    if (jsonLike.isDefined) {
      if (jsonLike.get.isString) {
        current = touchString(interactionId, normalizedTrailVisitor.wasStringTrails)
      }
      if (jsonLike.get.isNumber) {
        current = touchNumber(interactionId, normalizedTrailVisitor.wasNumberTrails)
      }
      if (jsonLike.get.isBoolean) {
        current = touchBoolean(interactionId, normalizedTrailVisitor.wasBooleanTrails)
      }
      if (jsonLike.get.isNull) {
        current = touchNull(interactionId, normalizedTrailVisitor.wasNullTrails)
      }
      if (jsonLike.get.isArray) {
        current = touchArray(interactionId, normalizedTrailVisitor.wasArrayTrails)
      }
      if (jsonLike.get.isObject) {
        current = touchObject(interactionId, normalizedTrailVisitor.wasObjectTrails)
      }
    } else {
      current = touchMissing(interactionId, normalizedTrailVisitor.wasMissingTrails)
    }
    current
  }

}
