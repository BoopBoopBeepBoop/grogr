package grogr.server.api

import grogr.core.model.Schema
import upickle.default.ReadWriter
import Protocol.*

import java.util.UUID

case class EngineDTO(
    drivers: Seq[String]
) derives ReadWriter

case class SessionCreateDTO(
    driver: String,
    args: Map[String, String]
) derives ReadWriter

case class SessionDTO(
    id: UUID,
    schema: Schema
) derives ReadWriter
