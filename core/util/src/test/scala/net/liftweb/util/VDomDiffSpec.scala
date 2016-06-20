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

    "find attributes which have been changed" in {
      val before = <div class="bold"></div>
      val after  = <div class="italics"></div>

      val expected = node(0).withPatches(VNodeAttrSet("class", "italics"))

      diff(0, before, after) must_== expected
    }

    "find attributes which have been added" in {
      val before = <div></div>
      val after  = <div class="italics"></div>

      val expected = node(0).withPatches(VNodeAttrSet("class", "italics"))

      diff(0, before, after) must_== expected
    }

    "find attributes which have been removed" in {
      val before = <div class="italics"></div>
      val after  = <div></div>

      val expected = node(0).withPatches(VNodeAttrRm("class"))

      diff(0, before, after) must_== expected
    }

    "ignore input elements which change Lift ID values as the name" in {
      val before = <input value="" id="chat-in" type="text" name="F273668743084VX1Y2H"/>
      val after  = <input value="" id="chat-in" type="text" name="F2736687431140WJQZB"/>

      val expected = node(0)

      diff(0, before, after) must_== expected
    }

    "ignore changes to id, which can happen with a form with a Lift ID" in {
      val before = <form id="F265092769361EVWYMA"></form>
      val after  = <form id="F265092769418MACZXI"></form>

      val expected = node(0)

      diff(0, before, after) must_== expected
    }

    "ignore any elements marked with data-lift-ignore-on-update" in {
      val before = <div data-lift-ignore-on-update="">Some stuff</div>
      val after  = <div data-lift-ignore-on-update="" attr="blah">Different stuff</div>

      val expected = node(0)

      diff(0, before, after) must_== expected
    }

  }
}
