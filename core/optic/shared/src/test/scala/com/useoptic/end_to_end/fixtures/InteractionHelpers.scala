package com.useoptic.end_to_end.fixtures

import java.util.Base64

import com.useoptic.types.capture.{ArbitraryData, Body, HttpInteraction, JsonLikeFrom, Request, Response}
import io.circe.Json
import optic_shape_hash.shapehash.JsonToShapeHash

object InteractionHelpers {

  def newInteraction(method: String, path: String, statusCode: Int, requestBody: Json = null, responseBody: Json = null) = {
    var interaction = HttpInteraction("id",
      Request(
      "example.com",
      method,
      path,
      ArbitraryData.empty,
      ArbitraryData.empty,
      Body.empty
    ),
      Response(
        statusCode,
        ArbitraryData.empty,
        Body.empty),
      Vector()
    )

    if (requestBody != null) {
      interaction = interaction.withRequestBody(requestBody)
    }
    if (responseBody != null) {
      interaction = interaction.withResponseBody(responseBody)
    }

    interaction
  }

  implicit class InteractionHelpers(interaction: HttpInteraction) {

    def withRequestBody(json: Json) = interaction.copy(request = interaction.request.copy(body = interaction.request.body.copy(
      contentType = Some("application/json"),
      value = ArbitraryData(
        shapeHashV1Base64 = Some(JsonToShapeHash.fromJson(JsonLikeFrom.json(json).get)).map(i => Base64.getEncoder().encodeToString(i.toByteString.toByteArray())),
        asText = Some(json.noSpaces), asJsonString = Some(json.noSpaces))
    )))

    def withResponseBody(json: Json) = interaction.copy(response = interaction.response.copy(body = interaction.response.body.copy(
      contentType = Some("application/json"),
      value = ArbitraryData(
        shapeHashV1Base64 = Some(JsonToShapeHash.fromJson(JsonLikeFrom.json(json).get)).map(i => Base64.getEncoder().encodeToString(i.toByteString.toByteArray())),
        asText = Some(json.noSpaces), asJsonString = Some(json.noSpaces))
    )))

    def forkRequestBody(fork: Json => Json) = {
      withRequestBody(fork(interaction.request.body.jsonOption.getOrElse(Json.obj())))
    }

    def forkResponseBody(fork: Json => Json) = {
      withResponseBody(fork(interaction.response.body.jsonOption.getOrElse(Json.obj())))
    }
  }

}
