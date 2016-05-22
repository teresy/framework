package net.liftweb.util

import org.specs2.mutable.Specification

import scala.xml.Node

object VDomDiffMatrixSpec extends Specification {
  import VDom.{ diffMatrix, VDomHelpers }
  import VDomHelpers.isntWhitespace
  import VDom.DiffMatrix

  "VDom.diffMatrix()".title

  def diff(a:Node, b:Node):DiffMatrix = diffMatrix(
    a.nonEmptyChildren.filter(isntWhitespace).toList,
    b.nonEmptyChildren.filter(isntWhitespace).toList
  )

  "VDom.diffMatrix" should {
    "be identity map when the trees match" in {
      val a = <ul>
        <li>Message 1</li>
        <li>Message 2</li>
      </ul>
      val expected = DiffMatrix(Map(0 -> (0, 1.0f), 1 -> (1, 1.0f)), Nil, Nil, Nil)

      diff(a, a) must_== expected
    }

    "identify something in a but not in b" in {
      val a = <ul>
        <li>Message 1</li>
        <li>Message 2</li>
      </ul>
      val b = <ul>
        <li>Message 2</li>
      </ul>
      val expected = DiffMatrix(Map(1 -> (0, 1.0f)), Nil, List(0), Nil)

      diff(a, b) must_== expected
    }

    "identify something in b but not in a" in {
      val a = <ul>
        <li>Message 2</li>
      </ul>
      val b = <ul>
        <li>Message 1</li>
        <li>Message 2</li>
      </ul>
      val expected = DiffMatrix(Map(0 -> (1, 1.0f)), List(0), Nil, Nil)

      diff(a, b) must_== expected
    }

    "identify an add, remove, and a move" in {
      val a = <ul>
        <li>Message 1</li>
        <li>Message 2</li>
      </ul>
      val b = <ul>
        <li>Message 2</li>
        <li>Message 3</li>
      </ul>
      val expected = DiffMatrix(Map(1 -> (0, 1.0f)), List(1), List(0), Nil)

      diff(a, b) must_== expected
    }

  }

}
