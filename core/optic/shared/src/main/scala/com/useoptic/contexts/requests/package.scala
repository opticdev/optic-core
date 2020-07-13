package com.useoptic.contexts.requests

import com.useoptic.contexts.requests.Requests.ContentType

import scala.scalajs.js.annotation.{JSExport, JSExportAll}
import scala.scalajs.js

package object Requests {
  @JSExportAll
  case class ContentType(value: String, supportsShape: Boolean)
}

@JSExport
@JSExportAll
object ContentTypes {
  val supported = Vector(
    ContentType("application/json", supportsShape = true),
    ContentType("application/x-www-form-urlencoded", supportsShape = true),
    ContentType("multipart/form-data", supportsShape = true),
    ContentType("application/octet-stream", supportsShape = false),
    ContentType("text/plain", supportsShape = false),
    ContentType("text/html", supportsShape = false),
  )

  private lazy val _contentTypeMap = supported.map(i => i.value -> i).toMap

}
