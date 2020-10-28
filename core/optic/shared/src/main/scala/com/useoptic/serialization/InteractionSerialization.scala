package com.useoptic.serialization

import com.useoptic.types.capture.{Body, HttpInteraction}
import io.circe.{Decoder, Json}

object InteractionSerialization {
  def fromJson(json: Json): HttpInteraction = {
    import io.circe.generic.auto._
    json.as[HttpInteraction].right.get
  }

  def bodyFromJson(json: Json): Body = {
    import io.circe.generic.auto._
    json.as[Body].right.get
  }
}
