package com.useoptic.diff.shapes

import com.useoptic.contexts.shapes.Commands.ShapeProvider
import com.useoptic.contexts.shapes.ShapesHelper.{ListKind, NumberKind, ObjectKind}
import com.useoptic.diff.initial.{DistributionAwareShapeBuilder, ShapeBuildingStrategy}
import com.useoptic.diff.interactions.TestHelpers
import org.scalatest.FunSpec
import com.useoptic.diff.shapes.JsonTrailPathComponent._
import com.useoptic.diff.shapes.resolvers.ShapesResolvers.ResolvedTrail
import com.useoptic.diff.shapes.resolvers.{DefaultShapesResolvers, JsonLikeResolvers}
import com.useoptic.dsa.OpticIds
import com.useoptic.types.capture.JsonLikeFrom
import io.circe.literal._


class ResolverSpec extends FunSpec {
  implicit val shapeBuildingStrategy = ShapeBuildingStrategy.inferPolymorphism
  describe("resolving trails") {
    describe("given a spec with a request body that is an object") {
      val builtShape = DistributionAwareShapeBuilder.toCommands(Vector(JsonLikeFrom.json(json"""{"f":[123]}""").get))(OpticIds.newDeterministicIdGenerator, shapeBuildingStrategy)
      val rfcState = TestHelpers.fromCommands(builtShape._2.flatten)
      val resolvers = new DefaultShapesResolvers(rfcState)

      it("should resolve the root as an object") {
        val resolvedTrail = resolvers.resolveTrailToCoreShape(ShapeTrail(builtShape._1, Seq()), Map.empty)
        println(resolvedTrail)
        assert(resolvedTrail == ResolvedTrail(rfcState.shapesState.shapes("shape_4"), ObjectKind, Map()))
      }

      it("should resolve the field 'f'") {
        val resolvedTrail = resolvers.resolveTrailToCoreShape(ShapeTrail(builtShape._1, Seq(ObjectFieldTrail("field_1", "shape_3"))), Map.empty)
        assert(resolvedTrail == ResolvedTrail(rfcState.shapesState.shapes("shape_3"), ListKind, Map(ListKind.innerParam -> Some(ShapeProvider("shape_2")))))
      }

      it("should resolve a value in field 'f'") {
        val resolvedTrail = resolvers.resolveTrailToCoreShape(ShapeTrail(builtShape._1, Seq(ObjectFieldTrail("field_1", "shape_3"), ListItemTrail("shape_3", "shape_2"))), Map.empty)

        assert(resolvedTrail == ResolvedTrail(rfcState.shapesState.shapes("shape_2"), NumberKind, Map(ListKind.innerParam -> Some(ShapeProvider("shape_2")))))
      }
    }
    describe("given a spec with a request body that is an array") {
      val builtShape = DistributionAwareShapeBuilder.toCommands(Vector(JsonLikeFrom.json(json"""[{"id": 1}]""").get))(OpticIds.newDeterministicIdGenerator, shapeBuildingStrategy)
      val rfcState = TestHelpers.fromCommands(builtShape._2.flatten)
      val resolvers = new DefaultShapesResolvers(rfcState)
      it("should resolve the root as a list") {
        val resolvedTrail = resolvers.resolveTrailToCoreShape(ShapeTrail(builtShape._1, Seq()), Map.empty)
        println(resolvedTrail)
        assert(resolvedTrail == ResolvedTrail(rfcState.shapesState.shapes(builtShape._1), ListKind, Map()))
      }
      it("should resolve the list item as an object") {
        println(builtShape._2)
        val resolvedTrail = resolvers.resolveTrailToCoreShape(ShapeTrail(builtShape._1, Seq(ListItemTrail("shape_5", "shape_4"))), Map.empty)
        assert(resolvedTrail == ResolvedTrail(rfcState.shapesState.shapes("shape_4"), ObjectKind, Map("$listItem" -> Some(ShapeProvider("shape_4")))))
      }
      it("should resolve the field id as a number") {
        val resolvedTrail = resolvers.resolveTrailToCoreShape(ShapeTrail(builtShape._1, Seq(ListItemTrail("shape_5", "shape_4"), ObjectFieldTrail("field_2", "shape_3"))), Map.empty)
        assert(resolvedTrail == ResolvedTrail(rfcState.shapesState.shapes("shape_3"), NumberKind, Map()))
      }
    }
  }


  describe("resolving json from trails and interactions") {
    describe("primitives") {
      it("should resolve the value") {

        val resolved = JsonLikeResolvers.tryResolveJsonTrail(JsonTrail(Seq()), JsonLikeFrom.json(json""""string""""))
        assert(resolved.get.asJson == json""""string"""")
      }
    }
    describe("object") {
      it("should resolve the value") {
        val resolved = JsonLikeResolvers.tryResolveJsonTrail(JsonTrail(Seq(JsonObject(), JsonObjectKey("k"))), JsonLikeFrom.json(json"""{"k":1}"""))
        assert(resolved.get.asJson == json"""1""")
      }
      it("should not resolve a value") {
        val resolved = JsonLikeResolvers.tryResolveJsonTrail(JsonTrail(Seq(JsonObject(), JsonObjectKey("notk"))), JsonLikeFrom.json(json"""{"k":1}"""))
        assert(resolved.isEmpty)
      }
    }
    describe("array") {
      it("should resolve the value") {
        val resolved = JsonLikeResolvers.tryResolveJsonTrail(JsonTrail(Seq(JsonArray(), JsonArrayItem(0))), JsonLikeFrom.json(json"""[1]"""))
        assert(resolved.get.asJson == json"""1""")
      }
      it("should not resolve a value") {
        val resolved = JsonLikeResolvers.tryResolveJsonTrail(JsonTrail(Seq(JsonArray(), JsonArrayItem(1))), JsonLikeFrom.json(json"""[1]"""))
        assert(resolved.isEmpty)
      }
    }
    describe("nested") {
      val nested = json"""{"k":[{"x": ["a", "b"]}]}"""
      it("should resolve .k[0].x[1] => 'b'") {
        val trail = JsonTrail(Seq(JsonObject(), JsonObjectKey("k"), JsonArray(), JsonArrayItem(0), JsonObject(), JsonObjectKey("x"), JsonArray(), JsonArrayItem(1)))
        val resolved = JsonLikeResolvers.tryResolveJsonTrail(trail, JsonLikeFrom.json(nested))
        assert(resolved.get.asJson == json""""b"""")
      }
      it("should not resolve .k[0].x[2]") {
        val trail = JsonTrail(Seq(JsonObject(), JsonObjectKey("k"), JsonArray(), JsonArrayItem(0), JsonObject(), JsonObjectKey("x"), JsonArray(), JsonArrayItem(2)))
        val resolved = JsonLikeResolvers.tryResolveJsonTrail(trail, JsonLikeFrom.json(nested))
        assert(resolved.isEmpty)
      }
    }
  }
}
