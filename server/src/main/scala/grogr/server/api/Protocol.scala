package grogr.server.api

import upickle.default.ReadWriter

import java.util.UUID

// readWriters that don't belong anywhere else
object Protocol {
  given uuidReadWriter: ReadWriter[UUID] = summon[ReadWriter[String]].bimap[UUID](_.toString, UUID.fromString)
}
