package com.useoptic

import com.useoptic.diff.shapes.JsonTrail
import com.useoptic.diff.shapes.JsonTrailPathComponent.{JsonArrayItem, JsonObjectKey}

import scala.scalajs.js.annotation.{JSExport, JSExportAll}


/*

const JsonTrailHelper = com.useoptic.JsonTrailHelper()

const rootTrail = JsonTrailFacade.emptyTrail

JsonTrailHelper.appendObjectKey('key1', rootTrail)



const shouldShowDiffForThisRow = !!trailsWithDiffs.find(i => JsonTrailHelper.compareEquality(i, thisRowsTrail)

*/

/*
 Alternatively...you can do your own implementation of JSON Trail, use an open source JS one, and we can make our domain impl map to that.
Dev and I prefer using arrays of case classes, to a template string since it's easier to work with and removes a class of parsing/splitting bug.

 */

@JSExport
@JSExportAll
object JsonTrailHelper {
  def emptyTrail: JsonTrail = JsonTrail(Seq.empty)
  def compareEquality(a: JsonTrail, b: JsonTrail) = a.path == b.path

  def appendObjectKey(key: String, trail: JsonTrail) = trail.withChild(JsonObjectKey(key))
  def appendArrayIndex(index: Int, trail: JsonTrail) = trail.withChild(JsonArrayItem(index))
}