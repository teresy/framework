package net.liftweb.util

import org.specs2.mutable.Specification

import VDom._
import VDomHelpers._
import VNode.{text => txt}

object VDomDiffSpec extends Specification {
  "VDom.diff() Specification".title

  "VDom.diff" should {
    import VDom.diff

    "find an appended element" in {
      val before =
        <div>
          <hr/>
          <ul>
            <li>Message 1</li>
            <li>Message 2</li>
          </ul>
        </div>

      val after =
        <div>
          <hr/>
          <ul>
            <li>Message 1</li>
            <li>Message 2</li>
            <li>Message 3</li>
          </ul>
        </div>

      val expected =
        node(0,
          node(1).withPatches(VNodeInsert(2, VNode("li", Map(), List(txt("Message 3")))))
        )

      diff(0, before, after) must_== expected
    }

    "find an inserted element" in {
      val before =
        <div>
          <hr/>
          <ul>
            <li>Message 1</li>
            <li>Message 2</li>
          </ul>
        </div>

      val after =
        <div>
          <hr/>
          <ul>
            <li>Message 1</li>
            <li>Message 3</li>
            <li>Message 2</li>
          </ul>
        </div>

      val expected =
        node(0,
          node(1).withPatches(VNodeInsert(1, VNode("li", Map(), List(txt("Message 3")))))
        )

      diff(0, before, after) must_== expected
    }

    "find a removed element" in {
      val before =
        <div>
          <hr/>
          <ul>
            <li>Message 1</li>
            <li>Message 2</li>
          </ul>
        </div>

      val after =
        <div>
          <hr/>
          <ul>
            <li>Message 2</li>
          </ul>
        </div>

      val expected =
        node(0,
          node(1).withPatches(VNodeDelete(0))
        )

      diff(0, before, after) must_== expected
    }

    "find a removed element identical to a sibling" in {
      val before =
        <div>
          <hr/>
          <ul>
            <li>Message 1</li>
            <li>Message 2</li>
            <li>Message 2</li>
          </ul>
        </div>

      val after =
        <div>
          <hr/>
          <ul>
            <li>Message 1</li>
            <li>Message 2</li>
          </ul>
        </div>

      val expected =
        node(0,
          node(1).withPatches(VNodeDelete(2))
        )

      diff(0, before, after) must_== expected
    }

    "find reordered elements" in {
      val before =
        <div>
          <hr/>
          <ul>
            <li>Message 1</li>
            <li>Message 2</li>
            <li>Message 3</li>
            <li>Message 4</li>
          </ul>
        </div>

      val after =
        <div>
          <hr/>
          <ul>
            <li>Message 2</li>
            <li>Message 4</li>
            <li>Message 3</li>
            <li>Message 1</li>
          </ul>
        </div>

      val expected =
        node(0,
          node(1).withPatches(VNodeReorder(List(0, 3, 1)))
        )

      diff(0, before, after) must_== expected
    }

    "find more reordered elements" in {
      val before =
        <div>
          <hr/>
          <ul>
            <li>Message 1</li>
            <li>Message 2</li>
          </ul>
        </div>

      val after =
        <div>
          <hr/>
          <ul>
            <li>Message 2</li>
            <li>Message 1</li>
          </ul>
        </div>

      val expected =
        node(0,
          node(1).withPatches(VNodeReorder(List(1, 0)))
        )

      diff(0, before, after) must_== expected
    }

    "find added and reordered elements" in {
      val before =
        <div>
          <hr/>
          <ul>
            <li>Message 1</li>
            <li>Message 2</li>
          </ul>
        </div>

      val after =
        <div>
          <hr/>
          <ul>
            <li>Message 2</li>
            <li>Message 1</li>
            <li>Message 3</li>
          </ul>
        </div>

      val expected =
        node(0,
          node(1).withPatches(VNodeInsert(2, VNode("li", Map(), List(txt("Message 3")))), VNodeReorder(List(1, 0)))
        )

      diff(0, before, after) must_== expected
    }

  }
}
