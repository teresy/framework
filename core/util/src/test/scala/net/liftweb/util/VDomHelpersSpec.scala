package net.liftweb.util

import VDom.VDomHelpers._

import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification

import scala.xml.Node

object VDomHelpersSpec extends Specification with XmlMatchers {
  "VDomHelpers Specification".title

  def nodesMust_==(a:Node, b:Node) = {
    withoutWhitespace(a) must_== withoutWhitespace(b)
  }

  "VDomHelpers.nodeCount" should {
    "count the number of nodes in an XML tree" in {
      val xml = <a>
        <b></b>
        <c></c>
        <d>
          <e><f></f></e>
          <g></g>
        </d>
        H
        <i>J</i>
      </a>

      nodeCount(xml) must_== 10
    }
  }

  "VDomHelpers.insertNode" should {
    "insert a node after the first child" in {
      val before = <div><span></span></div>
      val toAdd = <span>text</span>
      val after = <div><span></span>{toAdd}</div>

      nodesMust_==(insertNode(before, toAdd, 1, true), after)
    }

    "insert a node after the second child" in {
      val before = <div><span></span><div></div></div>
      val toAdd = <span>text</span>
      val after = <div><span></span><div></div>{toAdd}</div>

      nodesMust_==(insertNode(before, toAdd, 2, true), after)
    }

    "insert a grandchild node after the first" in {
      val before = <div>
        <ul>
          <li>msg 1</li>
          <li>msg 2</li>
          <li>msg 3</li>
        </ul>
      </div>

      val toAdd = <li>msg 4</li>

      val after = <div>
        <ul>
          <li>msg 1</li>
          {toAdd}
          <li>msg 2</li>
          <li>msg 3</li>
        </ul>
      </div>

      nodesMust_==(insertNode(before, toAdd, 2, true), after)
    }

    "insert a grandchild node after the second" in {
      val before = <div>
        <ul>
          <li>msg 1</li>
          <li>msg 2</li>
          <li>msg 3</li>
        </ul>
      </div>

      val toAdd = <li>msg 4</li>

      val after = <div>
        <ul>
          <li>msg 1</li>
          <li>msg 2</li>
          {toAdd}
          <li>msg 3</li>
        </ul>
      </div>

      nodesMust_==(insertNode(before, toAdd, 4, true), after)
    }
  }

  "VDomHelpers.removeNode" should {
    "remove the first child node" in {
      val before = <div><span></span></div>
      val after = <div></div>

      nodesMust_==(removeNode(before, 1), after)
    }

    "remove a second child" in {
      val before = <div><span></span><div></div></div>
      val after = <div><span></span></div>

      nodesMust_==(removeNode(before, 2), after)
    }

    "remove the first grandchild node" in {
      val before = <div>
        <ul>
          <li>msg 1</li>
          <li>msg 2</li>
          <li>msg 3</li>
        </ul>
      </div>

      val after = <div>
        <ul>
          <li>msg 2</li>
          <li>msg 3</li>
        </ul>
      </div>

      nodesMust_==(removeNode(before, 2), after)
    }

    "remove the second grandchild node" in {
      val before = <div>
        <ul>
          <li>msg 1</li>
          <li>msg 2</li>
          <li>msg 3</li>
        </ul>
      </div>

      val after = <div>
        <ul>
          <li>msg 1</li>
          <li>msg 3</li>
        </ul>
      </div>

      nodesMust_==(removeNode(before, 4), after)
    }

    "remove the last grandchild node" in {
      val before = <div>
        <ul>
          <li>msg 1</li>
          <li>msg 2</li>
          <li>msg 3</li>
        </ul>
      </div>

      val after = <div>
        <ul>
          <li>msg 1</li>
          <li>msg 2</li>
        </ul>
      </div>

      nodesMust_==(removeNode(before, 6), after)
    }
  }

  "VDomHelpers.recFilter" should {
    "filter all whitespace nodes when passed isntWhitespace" in {
      val before = <div>
        <ul>
          <li>msg 1</li>
          <li>msg 2</li>
          <li>msg 3</li>
        </ul>
      </div>

      val after = <div><ul><li>msg 1</li><li>msg 2</li><li>msg 3</li></ul></div>

      nodesMust_==(recFilter(before, isntWhitespace), after)
    }
  }

  "VDomHelpers.swapChildren" should {
    "switch two specified children in the tree" in {
      val before = <div>
        <ul>
          <li>msg 1</li>
          <li>msg 2</li>
          <li>msg 3</li>
        </ul>
      </div>

      val after = <div>
        <ul>
          <li>msg 3</li>
          <li>msg 2</li>
          <li>msg 1</li>
        </ul>
      </div>

      nodesMust_==(swapChildren(before, 1, 0, 2), after)
    }
  }

}
