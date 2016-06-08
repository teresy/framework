package net.liftweb.util

import net.liftweb.json._

import scala.annotation.tailrec
import scala.xml._

object VDom {
  import VDomHelpers._
  case class VNode(tag:String, attributes:Map[String, String] = Map(), children:List[VNode] = List(), text:Option[String] = None)

  // TODO: Attribute updates
  trait VNodePatch
  case class VNodeInsert(index:Int, node:VNode) extends VNodePatch
  case class VNodeDelete(index:Int) extends VNodePatch
  case class VNodeReorder(permutation:List[Int]) extends VNodePatch

  case class VNodePatchTree(index:Int, patches:List[VNodePatch], children:List[VNodePatchTree])

  object typeHints extends TypeHints {
    val classToHint:Map[Class[_], String] = Map(
      classOf[VNodeInsert] -> "insert",
      classOf[VNodeDelete] -> "delete",
      classOf[VNodeReorder] -> "reorder"
    )
    val hint2Class:Map[String, Class[_]] = classToHint.map { case (c, h) => h -> c }.toMap
    override val hints: List[Class[_]] = classToHint.keysIterator.toList
    override def hintFor(clazz: Class[_]):String = classToHint(clazz)
    override def classFor(hint: String) = hint2Class.get(hint)
  }
  val formats = new Formats {
    override val dateFormat: DateFormat = DefaultFormats.lossless.dateFormat
    override val typeHints = VDom.typeHints
    override val typeHintFieldName = "type"
  }

  private [util] case class DiffMatrix(matches:Map[Int, (Int, Float)], notInA: List[Int], notInB: List[Int])
  private [util] def diffMatrix(aChildren:List[Node], bChildren:List[Node]):DiffMatrix = {
    val compares = for {
      (a, i) <- aChildren.zipWithIndex
      (b, j) <- bChildren.zipWithIndex
    } yield (compare(a, b), i, j)
    val sortedCompares = compares.sortBy(_._1)

    val aWithMatchAndScore = sortedCompares.foldLeft(Map.empty[Int, (Int, Float)]) {
      case (acc, (score, a, b)) =>
        acc.find(_._2._1 == b) match {
          case Some((origA, (origB, origScore))) if origScore < score => (acc - origA) + (a -> (b, score))
          case None => acc + (a -> (b, score))
          case _ => acc
        }
    }
    val bWithMatchAndScore = sortedCompares.foldLeft(Map.empty[Int, (Int, Float)]) {
      case (acc, (score, a, b)) =>
        acc.find(_._2._1 == a) match {
          case Some((origB, (origA, origScore))) if origScore < score => (acc - origB) + (b -> (a, score))
          case None => acc + (b -> (a, score))
          case _ => acc
        }
    }
    val matches = aWithMatchAndScore.collect {
      case (a, (b, score)) if score > 0.0f => a -> (b, score)
    }
    val notInA = bWithMatchAndScore.collect {
      case (b, (a, score)) if score <= 0.0f => b
    }.toList ++ bChildren.zipWithIndex.collect {
      case (_, i) if !bWithMatchAndScore.keySet.contains(i) => i
    }
    val notInB = aWithMatchAndScore.collect {
      case (a, (b, score)) if score <= 0.0f => a
    }.toList ++ aChildren.zipWithIndex.collect {
      case (_, i) if !aWithMatchAndScore.keySet.contains(i) => i
    }

    DiffMatrix(matches, notInA, notInB)
  }

  def diff(index:Int, a:Node, b:Node):VNodePatchTree = {
    val aChildren = a.nonEmptyChildren.filter(isntWhitespace).toList
    val bChildren = b.nonEmptyChildren.filter(isntWhitespace).toList

    val matrix = diffMatrix(aChildren, bChildren)

    val matches = matrix.matches.map {
      case(a, (b, score)) => a -> b
    }

    val matchesAdjustedForAdditions:Map[Int, Int] = matrix.notInA.foldLeft(matches) {
      case (acc, i) => acc.map {
        case (j, k) => if (i > j) (j, k) else (j + 1, k)
      }
    }
    val matchesAdjusted:Map[Int, Int] = matrix.notInB.foldLeft(matchesAdjustedForAdditions) {
      case (acc, i) => acc.map {
        case (j, k) => if (i > j) (j, k) else (j - 1, k)
      }
    }

    val cycles = matchesAdjusted.foldRight(List(List.empty[Int])) { (z, maps:List[List[Int]]) =>
      val cycleList:List[List[Int]] = if (z._1 == z._2) maps
      else List((List(z._1, z._2):::maps.head).distinct)
      cycleList
    }

    val reorders = if (cycles == Nil) Nil else cycles.collect {
        case r if r.length==2 => VNodeReorder(r.reverse)
        case r => VNodeReorder(r)
    }
    val additions = matrix.notInA.map { i => VNodeInsert(i, VNode.fromXml(bChildren(i))) }
    val removals  = matrix.notInB.map { i => VNodeDelete(i) }.reverse

    val patches = removals ++ additions ++ reorders.filterNot(_==VNodeReorder(List()))

    val children = matrix.matches.toList.sortBy(_._1).collect {
      case (ai, (bi, score)) if score < 1.0f || aChildren(ai) != bChildren(bi) => diff(bi, aChildren(ai), bChildren(bi)) // The != is necessary for the case where equal ids made the match == 1.0f
    }

    VNodePatchTree(index, patches, children)
  }

  def hasSameId(a:Node, b:Node):Boolean = {
    def getId(n:Node) = n.attributes.collectFirst { case UnprefixedAttribute("id", Text(v), _) => v }
    val aId = getId(a)
    val bId = getId(b)

    aId.isDefined && aId == bId
  }

  def compare(a:Node, b:Node):Float = {
    if (a eq b) 1f
    else if (a.label != b.label) 0f
    else if (a.label == pcdata) if (a.text == b.text) 1f else 0f
    else if (hasSameId(a, b)) 1f
    else {
      // Compare children
      val aChildren = a.nonEmptyChildren.filter(isntWhitespace).toList
      val bChildren = b.nonEmptyChildren.filter(isntWhitespace).toList
      val matrix = diffMatrix(aChildren, bChildren)
      val sum = matrix.matches.foldLeft(0.0f) { case (acc, (_, (_, score))) => acc + score }
      val length = Math.max(aChildren.length, bChildren.length)
      if (length > 0) sum / length else 1.0f
    }
  }

  object VNode {
    def text(t:String):VNode = VNode("#text", Map(), List(), Some(t))
    def fromXml(n:Node):VNode = {
      if(n.label == pcdata) text(n.text)
      else {
        val attrs:Map[String, String] = n.attributes
          .collect { case UnprefixedAttribute(k, Text(v), _) => k -> v }
          .toMap
        val children:List[VNode] = n.nonEmptyChildren
          .filter(isntWhitespace)
          .map(fromXml)
          .toList

        VNode(n.label, attrs, children)
      }
    }
  }

  object VDomHelpers extends VDomHelpers
  trait VDomHelpers {
    val pcdata = "#PCDATA"
    def isText(n:Node) = n.label == pcdata
    def isWhitespace(n:Node)   = isText(n) && n.text.trim.isEmpty
    def isntWhitespace(n:Node) = !isWhitespace(n)

    def node(index:Int, child:VNodePatchTree*):VNodePatchTree = VNodePatchTree(index, List(), child.toList)
    def text(t:String) = VNode(pcdata, Map(), List(), Some(t))

    implicit class EnhancedVNodeTransformTree(t:VNodePatchTree) {
      def withPatches(patches:VNodePatch*) = t.copy(patches = patches.toList)
    }

    def nodeCount(n:Node):Int = {
      @tailrec
      def rec(ns:List[Node], acc:Int):Int = ns match {
        case n :: rest => rec(rest ++ n.nonEmptyChildren.filter(isntWhitespace), acc + 1)
        case Nil => acc
      }

      rec(List(n), 0)
    }

    def recFilter(n:Node, pred:Node => Boolean):Node = n match {
      case Elem(prefix, label, attributes, scope, children @ _*) =>
        Elem(prefix, label, attributes, scope, true, children.filter(pred).map(recFilter(_, pred)):_*)
      case _ => n
    }

    def withoutWhitespace(n:Node) = recFilter(n, isntWhitespace)

    private [this] def traverseUpdate(root:Node, atIndex:Int, siblingsAfterFound:(Node, List[Node]) => List[Node]):Node = {
      def rec(parent: Elem, siblingsBefore: List[Node], child: Node, siblingsAfter: List[Node], index: Int): (Node, Int) =
        (parent, child, child.nonEmptyChildren.filter(isntWhitespace).toList, siblingsAfter) match {
          // We found our node by index, so insert
          case (Elem(prefix, label, attributes, scope, _@_*), _, _, _) if index == atIndex =>
            (Elem(prefix, label, attributes, scope, true, siblingsBefore ++ siblingsAfterFound(child, siblingsAfter): _*), -1)

          // Child itself has children
          case (Elem(prefix, label, attributes, scope, _@_*), e: Elem, first :: rest, _) =>
            val (updatedChild, updatedIndex) = rec(e, Nil, first, rest, index + 1)
            if (updatedIndex < 0) (Elem(prefix, label, attributes, scope, true, siblingsBefore ++ (updatedChild :: siblingsAfter): _*), -1)
            else siblingsAfter match {
              case next :: rest if updatedIndex == atIndex => (Elem(prefix, label, attributes, scope, true, siblingsBefore ++ (child :: siblingsAfterFound(next, rest)): _*), -1)
              case next :: rest => rec(parent, siblingsBefore :+ child, next, rest, updatedIndex)
              case Nil => (parent, updatedIndex)
            }

          // We have more siblings to sift through
          case (_, _, _, next :: rest) =>
            rec(parent, siblingsBefore :+ child, next, rest, index + 1)

          // We have no children or siblings to check
          case _ => (parent, index + 1)
        }

      (root, root.nonEmptyChildren.filter(isntWhitespace).toList) match {
        case (e: Elem, first :: rest) => rec(e, Nil, first, rest, 1)._1
        case _ => root
      }
    }

    def insertNode(root:Node, newChild:Node, atIndex:Int, after:Boolean):Node = {
      def siblingsAfterFound(child:Node, siblingsAfter:List[Node]):List[Node] = child :: newChild :: siblingsAfter
      traverseUpdate(root, atIndex, siblingsAfterFound)
    }

    def removeNode(root:Node, atIndex:Int):Node = {
      def siblingsAfterFound(child:Node, siblingsAfter:List[Node]):List[Node] = siblingsAfter
      traverseUpdate(root, atIndex, siblingsAfterFound)
    }

    def swapChildren(root:Node, atIndex:Int, firstChild:Int, secondChild:Int):Node = {
      def siblingsAfterFound(child:Node, siblingsAfter:List[Node]):List[Node] = {
        val updated = child match {
          case Elem(prefix, label, attributes, scope, _@_*) =>
            val children = child.nonEmptyChildren.filter(isntWhitespace).toList

            val l1 = children.take(firstChild)
            val c1 = children(firstChild)
            val l2 = children.drop(firstChild + 1).take(secondChild - firstChild - 1)
            val c2 = children.drop(firstChild + 1).apply(secondChild - firstChild - 1)
            val l3 = children.drop(secondChild + 1)

            val updatedChildren = (l1 :+ c2) ++ l2 ++ (c1 +: l3)

            Elem(prefix, label, attributes, scope, true, updatedChildren:_*)
          case _ => child
        }

        updated :: siblingsAfter
      }
      traverseUpdate(root, atIndex, siblingsAfterFound)
    }

    def nodeAt(root:Node, atIndex:Int):Option[Node] = {
      def rec(child:Node, currentIndex:Int):(Int, Option[Node]) = {
        val children = child.nonEmptyChildren.filter(isntWhitespace)

        children.foldLeft((currentIndex + 1, Option.empty[Node])) {
          case ((_, opt @ Some(_)), next) => (-1, opt)
          case ((`atIndex`, None), next)  => (-1, Some(next))
          case ((i, None), next)          => rec(next, i)
        }
     }

      if(atIndex == 0) Some(root)
      else rec(root, 0)._2
    }
  }

}
