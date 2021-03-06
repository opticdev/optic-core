package com.useoptic.types.capture

import io.circe.{Json, JsonObject}
import io.circe.parser.parse
import optic_shape_hash.shapehash.{JsonToShapeHash, ShapeDescriptor}
import optic_shape_hash.shapehash.ShapeDescriptor.PrimitiveType

import scala.util.Try

abstract class JsonLike {
  def isString: Boolean

  def isBoolean: Boolean

  def isNumber: Boolean

  def isNull: Boolean

  def isArray: Boolean

  def isObject: Boolean

  def fields: Map[String, JsonLike]

  def items: Vector[JsonLike]
  def distinctItemsByIndex: Vector[(Int, JsonLike)]

  def shapeDescription: Option[ShapeDescriptor] = None
  def asJson: Json
}

object JsonLikeFrom {
  // real json
  def rawJson(text: String): Option[JsonLike] = parse(text).toOption.flatMap(JsonLikeFrom.json)

  def json(json: Json): Option[JsonLike] = Some(new JsonLike {
    override def isString: Boolean = json.isString

    override def isBoolean: Boolean = json.isBoolean

    override def isNumber: Boolean = json.isNumber

    override def isNull: Boolean = json.isNull

    override def isArray: Boolean = json.isArray

    override def isObject: Boolean = json.isObject

    override def fields: Map[String, JsonLike] = {
      json.asObject.getOrElse(JsonObject.empty).toMap.map {
        case (key, value) => (key, JsonLikeFrom.json(value).get)
      }
    }

    override def items: Vector[JsonLike] =
      json.asArray.getOrElse(Vector.empty)
        .map(i => JsonLikeFrom.json(i).get)

    override def asJson: Json = json

    override def distinctItemsByIndex: Vector[(Int, JsonLike)] = {
      val jsonHashItems = items.map(i => (JsonToShapeHash.fromJson(i), i.asJson))
        .flatMap(i => JsonLikeFrom.shapeHashDescriptor(i._1, Some(i._2)))

      val itemWithIndex = jsonHashItems
        .zipWithIndex

      itemWithIndex.map(_._1).distinct.map((uniqueHash:JsonLike) => {
        (itemWithIndex.find(_._1 == uniqueHash).get._2, uniqueHash)
      }).sortBy(_._1)

    }

    override def equals(obj: Any): Boolean = obj match {
      case other: JsonLike => {
        other.asJson == this.asJson
      }
      case _ => false
    }
  })

  //shape hash
  def rawShapeHash(bytes: Vector[Byte]): Option[JsonLike] = Try {
    val shapeDescriptor = ShapeDescriptor.parseFrom(bytes.toArray)
    shapeHashDescriptor(shapeDescriptor, None)
  }.toOption.flatten

  def rawShapeHashWithRawJson(bytes: Vector[Byte], rawJson: String): Option[JsonLike] = Try {
    val shapeDescriptor = ShapeDescriptor.parseFrom(bytes.toArray)
    shapeHashDescriptor(shapeDescriptor, parse(rawJson).toOption)
  }.toOption.flatten

  def shapeHashDescriptor(shapeDescriptor: ShapeDescriptor, realJson: Option[Json]): Option[JsonLike] = Some(new JsonLike {

    override def equals(obj: Any): Boolean = obj match {
      case other: JsonLike => other.shapeDescription.contains(shapeDescriptor)
      case _ => false
    }

    override def shapeDescription: Option[ShapeDescriptor] = Some(shapeDescriptor)

    override def distinctItemsByIndex: Vector[(Int, JsonLike)] = {
      val itemWithIndex = items
        .zipWithIndex

      itemWithIndex.map(_._1).distinct.map((uniqueHash:JsonLike) => {
        (itemWithIndex.find(_._1 == uniqueHash).get._2, uniqueHash)
      }).sortBy(_._1)
    }

    override def isString: Boolean = shapeDescriptor.`type`.isString

    override def isBoolean: Boolean = shapeDescriptor.`type`.isBoolean

    override def isNumber: Boolean = shapeDescriptor.`type`.isNumber

    override def isNull: Boolean = shapeDescriptor.`type`.isNull

    override def isArray: Boolean = shapeDescriptor.`type`.isArray

    override def isObject: Boolean = shapeDescriptor.`type`.isObject

    override def fields: Map[String, JsonLike] = {
      shapeDescriptor.fields.collect {
        case i if i.hash.isDefined => i.key -> {
          val realFieldJson = Try(realJson.get.asObject.get.toMap(i.key)).toOption
          shapeHashDescriptor(i.hash.get, realFieldJson).get
        }
      }.toMap
    }

    override def items: Vector[JsonLike] = {
      shapeDescriptor.items.zipWithIndex.map {
        case (item, index) => {
          val realFieldJson = Try(realJson.get.asArray.get(index)).toOption
          shapeHashDescriptor(item, realFieldJson)
        }
      }.toVector.flatten
    }

    override def asJson: Json = {
      if (realJson.isDefined) {
        realJson.get
      } else {
        shapeDescriptor match {
          case x if x.`type`.isString => Json.fromString("string")
          case x if x.`type`.isBoolean => Json.fromBoolean(true)
          case x if x.`type`.isNumber => Json.fromBigInt(1)
          case x if x.`type`.isNull => Json.Null
          case x if x.`type`.isArray => Json.fromValues(items.map(_.asJson))
          case x if x.`type`.isObject => Json.fromFields(fields.map {
            case (key, value) => (key -> value.asJson)
          })
        }
      }

    }
  })

  def knownText(text: String): Option[JsonLike] = Some(new JsonLike {
    override def isString: Boolean = true

    override def isBoolean: Boolean = false

    override def isNumber: Boolean = false

    override def isNull: Boolean = false

    override def isArray: Boolean = false

    override def isObject: Boolean = false

    override def fields: Map[String, JsonLike] = Map.empty

    override def items: Vector[JsonLike] = Vector.empty
    override def distinctItemsByIndex: Vector[(Int, JsonLike)] = Vector.empty

    override def asJson: Json = Json.fromString(text)
  })

}
