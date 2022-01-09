package grogr.clickhouse

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.*
import sttp.client3.*

import concurrent.ExecutionContext.Implicits.global

object CreateSampleData {
  def main(args: Array[String]): Unit = {
    val client = Clickhouse(uri"http://localhost:8123")

    val creates = Seq(
      "CREATE TABLE IF NOT EXISTS city_groups (city_id Int64, group String) engine = Log",
      "CREATE TABLE IF NOT EXISTS city (id Int64, name String) engine = Log",
      "CREATE TABLE IF NOT EXISTS pop1980(id Int64, pop Int64) engine = Log",
      "CREATE TABLE IF NOT EXISTS pop2000(id Int64, pop Int64) engine = Log",
      "CREATE TABLE IF NOT EXISTS city_groups_uniq (group String) engine = Log",
      "CREATE TABLE IF NOT EXISTS combined (id Int64, name String, pop1980 Int64, pop2000 Int64, group String) engine = Log"
    )

    val inserts = Seq(
      """INSERT INTO city_groups_uniq VALUES
        |  ('World'), ('USA')""".stripMargin,
      """INSERT INTO city_groups VALUES
        |(0, 'World'),
        |(1, 'World'),
        |(2, 'USA'),
        |(3, 'World'),
        |(4, 'USA'),
        |(5, 'World'),
        |(6, 'USA')""".stripMargin,
      """INSERT INTO city VALUES
        | (0, 'Tokyo'),
        | (1, 'Mumbai'),
        | (2, 'New York'),
        | (3, 'Paris'),
        | (4, 'Paris'),
        | (5, 'London'),
        | (6, 'London')""".stripMargin,
      """INSERT INTO pop1980 VALUES
        |(0, 12000000),
        |(1, 7520000),
        |(2, 8000000),
        |(3, 3400000),
        |(4, 12000)
        |(5, 1700000)
        |(6, 4700)
        |
        |""".stripMargin,
      """INSERT INTO pop2000 VALUES
        |(0, 13500000),
        |(1, 15740000),
        |(2, 9100000),
        |(3, 3280000),
        |(4, 13000)
        |(5, 1900000)
        |(6, 4200)""".stripMargin,
      """
        |INSERT INTO combined
        |  SELECT
        |    c.id, c.name, p1.pop, p2.pop, cg.group
        |  FROM
        |    city c
        |    JOIN city_groups cg ON c.id = cg.city_id
        |    JOIN pop1980 p1 ON c.id = p1.id
        |    JOIN pop2000 p2 ON c.id = p2.id
        |""".stripMargin
    )

    val foo = Await.result(Future.sequence(creates.map(client.statement)), 5.seconds)
    val foo2 = Await.result(Future.sequence(inserts.map(client.statement)), 5.seconds)

  }
}
