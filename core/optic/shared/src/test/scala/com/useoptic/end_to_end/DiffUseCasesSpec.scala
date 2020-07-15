package com.useoptic.end_to_end

import com.useoptic.diff.JsonFileFixture
import com.useoptic.diff.shapes.JsonTrailPathComponent.JsonObject
import com.useoptic.dsa.OpticIds
import com.useoptic.end_to_end.fixtures.InteractionHelpers._
import com.useoptic.end_to_end.snapshot_task.{EndEndDiffTask, TestDataHelper}
import io.circe.Json
import io.circe.literal._


class DiffUseCasesSpec extends EndEndDiffTask with JsonFileFixture {

  ////////////////////////////////////////////////////////////////////////////////////
  /// Basic Objects
  val personInteraction = newInteraction("GET", "/users/1234/profile", 200,
    responseBody = json"""{ "firstName": "Aidan", "lastName": "C", "age": 26, "cities": ["San Fransisco", "New York", "Durham"]}""")

  val baselineEvents = {
    implicit val ids = OpticIds.newPrefixedDeterministicIdGenerator("baseline")
    new TestDataHelper("root-shape-is-object-with-keys").learnBaselineEvents(
      path = Vector("users", ":userId", "profile"),
      interactions = Vector(personInteraction))
  }

  when("no diff expected for basic objects", () => EndEndDiffTask.Input(baselineEvents._1, {
    Vector(personInteraction)
  }))


  when("a known field is missing", () => EndEndDiffTask.Input(baselineEvents._1, Vector(
      personInteraction.forkResponseBody(json => {
        val obj = json.asObject.get.remove("firstName")
        Json.fromJsonObject(obj)
      })
    )))

    when("a known field is provided the wrong shape", () => EndEndDiffTask.Input(baselineEvents._1, Vector(
      personInteraction.forkResponseBody(json => {
        Json.fromJsonObject(json.asObject.get.add("age", Json.fromString("not a number")))
      })
    )))

    when("an extra field is provided", () => EndEndDiffTask.Input(baselineEvents._1, Vector(
      personInteraction.forkResponseBody(json => {
        Json.fromJsonObject(json.asObject.get.add("favoriteColor", Json.fromString("Syracuse-Orange")))
      })
    )))

    when("an extra field is provided as an object", () => EndEndDiffTask.Input(baselineEvents._1, Vector(
      personInteraction.forkResponseBody(json => {
        Json.fromJsonObject(json.asObject.get.add("favoriteColor", json"""{ "first": "orange", "second": "red"}"""))
      })
    )))

    when("field is array of strings, and 1 item does not match expected type", () => EndEndDiffTask.Input(baselineEvents._1, Vector(
      personInteraction.forkResponseBody(json => {
        Json.fromJsonObject(json.asObject.get.add("cities", Json.fromValues(Seq(
          Json.fromString("San Fransisco"),
          Json.fromInt(17584),
          Json.fromString("Boston"),
        ))))
      })
    )))

    when("field is array of strings, and > 1 items does not match expected type", () => EndEndDiffTask.Input(baselineEvents._1, Vector(
      personInteraction.forkResponseBody(json => {
        Json.fromJsonObject(json.asObject.get.add("cities", Json.fromValues(Seq(
          Json.fromString("San Fransisco"),
          Json.fromInt(17584),
          Json.fromString("Boston"),
          Json.fromInt(16573),
          Json.fromString("Chicago"),
        ))))
      })
    )))


  ////////////////////////////////////////////////////////////////////////////////////
  /// Nested Objects with Optionals
  val firstCityInteraction = newInteraction("GET", "/locations/sf", 200,
    responseBody = json"""{ "location": { "principality": { "city": "San Fransisco", "population": 830000 } } }""")

  val secondCityInteraction = newInteraction("GET", "/locations/sf", 200,
    responseBody = json"""{ "location": { "principality": { "city": "San Fransisco", "population": 830000, "coordinates": {"lat": "37.7749° N", "long": "122.4194° W"} } } }""")

  val baselineCityEvents = {
    implicit val ids = OpticIds.newDeterministicIdGenerator
    new TestDataHelper("root-shape-is-object").learnBaselineEvents(
      path = Vector("locations", ":city"),
      interactions = Vector(firstCityInteraction, secondCityInteraction))
  }

  when("a new field is provided in a required nested object", () => EndEndDiffTask.Input(baselineCityEvents._1, {
    Vector(firstCityInteraction.forkResponseBody(j => {
      json"""{ "location": { "principality": { "motto": "Experientia Docet", "city": "San Fransisco", "population": 830000 } } }"""
    }))
  }))


  when("a new field is provided in an optional nested object", () => EndEndDiffTask.Input(baselineCityEvents._1, {
    Vector(secondCityInteraction.forkResponseBody(j => {
      json"""{ "location": { "principality": { "city": "San Fransisco", "population": 830000, "coordinates": {"format": "DMS", "lat": "37.7749° N", "long": "122.4194° W"} } } }"""
    }))
  }))

  when("a new field is provided as an empty array", () => EndEndDiffTask.Input(baselineCityEvents._1, {
    Vector(secondCityInteraction.forkResponseBody(j => {
      json"""{ "location": { "principality": { "city": "San Fransisco", "population": 830000, "array": [] } } }"""
    }))
  }))

  when("a new field is provided as an array with any contents", () => EndEndDiffTask.Input(baselineCityEvents._1, {
    Vector(secondCityInteraction.forkResponseBody(j => {
      json"""{ "location": { "principality": { "city": "San Fransisco", "population": 830000, "array": [1,2,3] } } }"""
    }))
  }))

  when("a primitive type is provided to an optional object", () => EndEndDiffTask.Input(baselineCityEvents._1, {
    Vector(firstCityInteraction.forkResponseBody(j => {
      json"""{ "location": { "principality": { "city": "San Fransisco", "population": 830000, "coordinates": "N/A" } } }"""
    }))
  }))

  when("an array type is provided to an optional object", () => EndEndDiffTask.Input(baselineCityEvents._1, {
    Vector(firstCityInteraction.forkResponseBody(j => {
      json"""{ "location": { "principality": { "city": "San Fransisco", "population": 830000, "coordinates": [1,2,3] } } }"""
    }))
  }))

  when("required fields are omitted in an optional object", () => EndEndDiffTask.Input(baselineCityEvents._1, {
    Vector(firstCityInteraction.forkResponseBody(j => {
      json"""{ "location": { "principality": { "city": "San Fransisco", "population": 830000, "coordinates": {  } } } }"""
    }))
  }))


  ////////////////////////////////////////////////////////////////////////////////////
  ///Optional Objects/Fields and Arrays
  val personWithStats = newInteraction("GET", "/users/1234/profile", 200,
    responseBody = json"""{"name":{"first":"Bob","last":"C"},"rivals":["user1","user2","user3"],"stats":{"rank":1}}""")

  val baselinePWSEvents = {
    implicit val ids = OpticIds.newDeterministicIdGenerator
    new TestDataHelper("root-shape-is-object-with-keys").learnBaselineEvents(
      path = Vector("users", ":userId", "profile"),
      interactions = Vector(personWithStats))
  }

  when("an required object field is ommitted", () => EndEndDiffTask.Input(baselinePWSEvents._1, {
    Vector(personWithStats.forkResponseBody(j => {
      json"""{"name":{"first":"Bob","last":"C"},"rivals":["user1","user2","user3"]}"""
    }))
  }))


  when("an required object field is null, suggests nullable", () => EndEndDiffTask.Input(baselinePWSEvents._1, {
    Vector(personWithStats.forkResponseBody(j => {
      json"""{"name":{"first":"Bob","last":"C"},"rivals":["user1","user2","user3"], "stats": null}"""
    }))
  }))

  when("an required object field is provided with a missing required field", () => EndEndDiffTask.Input(baselinePWSEvents._1, {
    Vector(personWithStats.forkResponseBody(j => {
      json"""{"name":{"first":"Bob","last":"C"},"rivals":["user1","user2","user3"], "stats": {} }"""
    }))
  }))

  // diff error -> JsonArrayItem issue
  when("an required object field is provided with an array", () => EndEndDiffTask.Input(baselinePWSEvents._1, {
    Vector(personWithStats.forkResponseBody(j => {
      json"""{"name":{"first":"Bob","last":"C"},"rivals":["user1","user2","user3"], "stats": [12,34] }"""
    }))
  }))

  when("a required array field has no items, no diff", () => EndEndDiffTask.Input(baselinePWSEvents._1, {
    Vector(personWithStats.forkResponseBody(j => {
      json"""{"name":{"first":"Bob","last":"C"},"rivals":[],"stats":{"rank":1}}"""
    }))
  }))

  when("a required array field of strings provided with an object", () => EndEndDiffTask.Input(baselinePWSEvents._1, {
    Vector(personWithStats.forkResponseBody(j => {
      json"""{"name":{"first":"Bob","last":"C"},"rivals":[{"food": "rice"}, {"food": "cookies"}],"stats":{"rank":1}}"""
    }))
  }))

  when("a required array field is an object", () => EndEndDiffTask.Input(baselinePWSEvents._1, {
    Vector(personWithStats.forkResponseBody(j => {
      json"""{"name":{"first":"Bob","last":"C"},"rivals":{"nemesis": "Brad"},"stats":{"rank":1}}"""
    }))
  }))

  ////////////////////////////////////////////////////////////////////////////////////
  /// Arrays
  val emptyArray = newInteraction("GET", "/events", 200,
    responseBody = json"""[]""")

  val baselineArrayEvents = {
    implicit val ids = OpticIds.newDeterministicIdGenerator
    new TestDataHelper("root-shape-is-array").learnBaselineEvents(
      path = Vector("events"),
      interactions = Vector(emptyArray))
  }


  when("array unknown is provided with no values", () => EndEndDiffTask.Input(baselineArrayEvents._1, {
    Vector(emptyArray.forkResponseBody(j => {
      json"""[]"""
    }))
  }))


  when("root array is provided with object", () => EndEndDiffTask.Input(baselineArrayEvents._1, {
    Vector(emptyArray.forkResponseBody(j => {
      json"""{"foo": "bar"}"""
    }))
  }))


  when("array unknown is provided with concrete values", () => EndEndDiffTask.Input(baselineArrayEvents._1, {
    Vector(emptyArray.forkResponseBody(j => {
      json"""[1,2,3,4,5]"""
    }))
  }))


  val objectArray = newInteraction("GET", "/people", 200,
    responseBody = json"""[{"name": "joe", "age": "thirty", "colors": ["red", "green", "yellow"]}, {"name": "joe", "age": 45}]""")

  val baselineObjectArrayEvents = {
    implicit val ids = OpticIds.newDeterministicIdGenerator
    new TestDataHelper("root-shape-is-array").learnBaselineEvents(
      path = Vector("people"),
      interactions = Vector(objectArray))
  }

  when("array with object listitem is provided with no values", () => EndEndDiffTask.Input(baselineObjectArrayEvents._1, {
    Vector(objectArray.forkResponseBody(j => {
      json"""[]"""
    }))
  }))

  // diff error
  when("array with object listitem is provided with one matching and one primitive", () => EndEndDiffTask.Input(baselineObjectArrayEvents._1, {
    Vector(objectArray.forkResponseBody(j => {
      json"""[{"name": "joe", "age": "thirty", "colors": ["red", "green", "yellow"]}, "hello"]"""
    }))
  }))


  when("array with object listitem is provided with one matching, no diff", () => EndEndDiffTask.Input(baselineObjectArrayEvents._1, {
    Vector(objectArray.forkResponseBody(j => {
      json"""[{"name": "joe", "age": "thirty", "colors": ["red", "green", "yellow"]}]"""
    }))
  }))

  when("array with object listitem is provided an empty sub array", () => EndEndDiffTask.Input(baselineObjectArrayEvents._1, {
    Vector(objectArray.forkResponseBody(j => {
      json"""[[]]"""
    }))
  }))


  //diff error!
  when("array with object listitem is provided an sub array of numbers", () => EndEndDiffTask.Input(baselineObjectArrayEvents._1, {
    Vector(objectArray.forkResponseBody(j => {
      json"""[[1,2,3]]"""
    }))
  }))

  when("deeply nested fields inside of arrays", () => {

    val events = eventsFrom("ergast")

    val raceResults = newInteraction("GET", "/api/f1/2019/drivers/max_verstappen/results", 200,
      responseBody = fromFile("race-result"))

    EndEndDiffTask.Input(events, Vector(raceResults))
  })


  // todo! Nullables, Unknown conversions

  runSuite
}
