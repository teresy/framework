package net.liftweb.http

import net.liftweb.util.VDom.VDomHelpers
import VDomHelpers._
import org.scalacheck.{Gen, Prop, Properties}
import Prop._

import scala.collection.immutable.::
import scala.xml.{Node, Text}

object VDomGen {
  import org.scalacheck.Gen._

  def genMutations(n:Node, removable:List[Int], hasSwappableChildren:List[Int]) = for {
    numMutations <- choose(1, 2)
    mutated <- (1 to numMutations).foldLeft(const(n)) {
      case (gNode, _) => gNode.flatMap(n => genMutation(n, removable, hasSwappableChildren))
    }
  } yield {
    mutated
  }

  def genMutation(n:Node, removable:List[Int], hasSwappableChildren:List[Int]) = for {
    mutation <- oneOf(genRemove(n, removable), genInsert(n)) // , genSwap(n, hasSwappableChildren)
  } yield {
    mutation
  }

  def genRemove(n:Node, removable:List[Int]) = for {
    index <- oneOf(removable)
  } yield {
    removeNode(n, index)
  }

  def genInsert(n:Node) = for {
    index <- choose(1, nodeCount(n) - 1)
    newSibling <- genNode
  } yield {
    insertNode(n, newSibling, index, true)
  }

  def genNode:Gen[Node] = for {
    tag <- oneOf("p", "br", "hr", pcdata)
    str <- alphaStr if str.length > 1 // Part of the workaround explained in VDomHelper.insertNode
  } yield {
    if(tag == pcdata) Text(str)
    else <xml></xml>.copy(label = tag)
  }

  def genSwap(n:Node, hasSwappableChildren:List[Int]):Gen[Node] = for {
    index <- oneOf(hasSwappableChildren)
    swapped <- genSwapPrivate(n, index)
  } yield {
    swapped
  }

  private def genSwapPrivate(n:Node, index:Int):Gen[Node] = {
    val maybeChild = nodeAt(n, index)
    val maybeCount = maybeChild.map(_.nonEmptyChildren.filter(isntWhitespace).size)

    maybeChild.zip(maybeCount).headOption.map { case (child, count) =>
      for {
        pair <- pick(2, (0 to (count - 1)))
      } yield {
        val firstChild :: secondChild :: Nil = pair.toList
        swapChildren(n, index, firstChild, secondChild)
      }
    }.getOrElse(n)
  }
}

object UpdateDOMProperties extends Properties("UpdateDOM") {
  val template = recFilter(
    <body data-lift-content-id="main">
      <div id="main" data-lift="surround?with=default;at=content">
        <h2>Welcome to chat</h2>
        <span>Say something!</span>
        <form method="post" data-lift="form.ajax">
          <div data-lift="Chat.submit">
            <input type="text" id="chat-in" name="in"/>
            <input type="submit" value="Submit"/>
          </div>
        </form>
        <div>
          <ul data-lift="Chat.messages">
            <li class="chat-message">Message 1</li>
            <li class="chat-message clearable">Message 2</li>
            <li class="chat-message clearable">Message 3</li>
          </ul>
        </div>
      </div>
    </body>,
    isntWhitespace)

  property("UpdateDOM should handle an arbitrary mutation of our static template") = forAll(
    VDomGen.genMutations(template, List(2, 3, 4, 5, 10, 11, 12, 13, 14, 15, 16), List(1, 11))
  ) { after =>
    UpdateDOMSpec.roundTrip(template, after) == after
  }
}
