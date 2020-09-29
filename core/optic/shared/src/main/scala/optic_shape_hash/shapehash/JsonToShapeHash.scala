package optic_shape_hash.shapehash

import com.useoptic.types.capture.JsonLike
import optic_shape_hash.shapehash.ShapeDescriptor.PrimitiveType

object JsonToShapeHash {
  def fromJson(json: JsonLike): ShapeDescriptor = {
    var h = ShapeDescriptor.defaultInstance

    if (json.isString) {
      h = h.withType(PrimitiveType.STRING)
    } else if (json.isNumber) {
      h = h.withType(PrimitiveType.NUMBER)
    } else if (json.isNull) {
      h = h.withType(PrimitiveType.NULL)
    } else if (json.isBoolean) {
      h = h.withType(PrimitiveType.BOOLEAN)
    } else if (json.isArray) {
      h = h.withType(PrimitiveType.ARRAY)
      h = h.withItems(json.items.map(fromJson))
    } else if (json.isObject) {
      h = h.withType(PrimitiveType.OBJECT)
      h = h.withFields(json.fields.map(field => {
        val (key, value) = field
        var f = FieldDescriptor.defaultInstance
        f = f.withKey(key)
        f = f.withHash(fromJson(value))
        f
      }).toSeq)
    }

    h
  }
}
