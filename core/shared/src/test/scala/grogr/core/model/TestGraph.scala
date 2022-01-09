package grogr.core.model

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

case class TestValue(id: String, props: Set[String])

class TestGraph extends AnyFunSpec with Matchers {

  it("should build a graph") {

    val value1 = TestValue("one", props = Set("a1", "b1"))
    val value2 = TestValue("two", props = Set("c2", "d2"))
    val value3 = TestValue("three", props = Set("e3", "f3"))

    val graph = PropGraph[TestValue](_.id, _.props) { g =>
      g += value1
      g += value2
      g += value3

      g(value1).link("a1", g(value2) -> "d2")
      g(value2).link("c2", g(value3) -> "f3")
    }

    val path = graph.shortestPathBetween(value1, value3)
    path.get shouldEqual Seq(
      (value1, "a1", value2, "d2"),
      (value2, "c2", value3, "f3")
    )
  }

  describe("shortest path multiple") {
    val value1 = TestValue("one", props = Set("a1", "b1"))
    val value2 = TestValue("two", props = Set("c2", "d2"))
    val value3 = TestValue("three", props = Set("e3", "f3"))
    val value4 = TestValue("four", props = Set("g4", "h4"))
    val value5 = TestValue("five", props = Set("i5", "j5"))

    val graph = PropGraph[TestValue](_.id, _.props) { g =>
      Seq(value1, value2, value3, value4, value5).foreach { g += _ }

      // long single connections
      g(value1).link("a1", g(value2) -> "d2")
      g(value2).link("c2", g(value3) -> "f3")
      g(value3).link("f3", g(value4) -> "g4")
      g(value4).link("g4", g(value5) -> "j5")

      // also add a short path
      g(value1).link("b1", g(value5) -> "i5")
    }

    it("should find shortest path between multiple [1]") {
      val path = graph.shortestPathBetweenAny(Set(value1, value2), Set(value5))
      path.get shouldEqual Seq(
        (value1, "b1", value5, "i5")
      )
    }

    it("should find shortest path between multiple [2]") {
      val path2 = graph.shortestPathBetweenAny(Set(value1), Set(value3))
      path2.get shouldEqual Seq(
        (value1, "a1", value2, "d2"),
        (value2, "c2", value3, "f3")
      )
    }

    it("should find shortest path between multiple [3]") {
      val path3 = graph.shortestPathBetweenAny(Set(value1), Set(value2, value3))
      path3.get shouldEqual Seq(
        (value1, "a1", value2, "d2")
      )
    }
  }
}
