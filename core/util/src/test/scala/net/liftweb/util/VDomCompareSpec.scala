package net.liftweb.util

import org.specs2.mutable.Specification

import scala.xml.Text

object VDomCompareSpec extends Specification {
  "VDom.compare()".title

  "VDom.compare" should {
    import VDom.compare

    "regard elements with different tags as dissimilar" in {
      val a = <div></div>
      val b = <span></span>
      compare(a, b) must_== 0f
    }

    "regard elements with different tags as dissimilar even if have same id" in {
      val a = <div id="nope"></div>
      val b = <span id="nope"></span>
      compare(a, b) must_== 0f
    }

    "regard elements with the same tags and children as the same" in {
      val a = <span>Some text</span>
      val b = <span>Some text</span>  // Purposefully making a copy for the test
      compare(a, b) must_== 1f
    }

    "regard elements with the same tags but different child text nodes as different" in {
      val a = <span>Some text</span>
      val b = <span>Some other text</span>
      compare(a, b) must_== 0.0f
    }

    "regard two Text nodes with the same text as the same" in {
      val a = Text("text")
      val b = Text("text")
      compare(a, b) must_== 1f
    }

    "regard two Text nodes with different text as different" in {
      val a = Text("text")
      val b = Text("blah")
      compare(a, b) must_== 0f
    }

    "regard elements with the same tags and no children as the same" in {
      val a = <div></div>
      val b = <div></div>
      compare(a, b) must_== 1f
    }

    "regard elements with the same tags but different children as a ratio of similar children" in {
      val a =
        <ul>
          <li>One</li>
          <li>Two</li>
        </ul>
      val b =
        <ul>
          <li>One</li>
          <li>Three</li>
        </ul>

      compare(a, b) must_== 0.5f
    }

    "regard elements with the same tags but different number children as a ratio of similar children counting trailing absentees as dissimilar" in {
      val a =
        <ul>
          <li>One</li>
        </ul>
      val b =
        <ul>
          <li>One</li>
          <li>Two</li>
        </ul>

      compare(a, b) must_== 0.5f
    }

    "regard elements with the same tags but different number children as a ratio of similar children counting leading absentees as dissimilar" in {
      val a =
        <ul>
          <li>One</li>
          <li>Two</li>
        </ul>
      val b =
        <ul>
          <li>Two</li>
        </ul>

      compare(a, b) must_== 0.5f
    }

    "regard elements with the same tags but one with children and one without as dissimilar" in {
      val a =
        <ul>
        </ul>
      val b =
        <ul>
          <li>One</li>
          <li>Two</li>
        </ul>

      compare(a, b) must_== 0.0f
    }

    "regard elements who have the same children but in different order as similar" in {
      val a =
        <ul>
          <li>Message 1</li>
          <li>Message 2</li>
          <li>Message 3</li>
          <li>Message 4</li>
        </ul>
      val b =
        <ul>
          <li>Message 2</li>
          <li>Message 4</li>
          <li>Message 3</li>
          <li>Message 1</li>
        </ul>

      compare(a, b) must_== 1.0f
    }

    "regard elements as the same if they both have the same attributes" in {
      val a = <input type="text" name="in"/>
      val b = <input type="text" name="in"/>

      compare(a, b) must_== 1.0f
    }

    "regard elements with different attributes as dissimilar" in {
      val a = <div class="text"/>
      val b = <div class="submit"/>

      compare(a, b) must_== 0.0f
    }

    "regard elements with similiarity as a ratio of children and attributes in common" in {
      val a = <div class="blah" data-lift="bleh">
        <span>One</span>
        <span>Two</span>
      </div>
      val b = <div class="garbage" data-lift="bleh">
        <span>One</span>
        <span>Two</span>
        <span>Three</span>
      </div>

      compare(a, b) must_== 0.6f
    }

    "regard elements with different ids as dissimilar" in {
      val a = <span id="same">same</span>
      val b = <span id="different">same</span>
      compare(a, b) must_== 0f
    }

    "regard elements as dissimilar if one has an id and the other doesn't" in {
      val a = <span id="same">same</span>
      val b = <span>same</span>
      compare(a, b) must_== 0f
    }

    "regard elements with same tags and ids as the same regardless of children" in {
      val a = <span id="same">Not</span>
      val b = <span id="same">Different</span>
      compare(a, b) must_== 1f
    }

    "regard two input elements as the same if both type and name are the same" in {
      val a = <input type="text" name="bob"/>
      val b = <input type="text" name="bob"/>
      compare(a, b) must_== 1f
    }

    "regard two input elements as the dissimilar if the names are different" in {
      val a = <input type="text" name="bob"/>
      val b = <input type="text" name="jill"/>
      compare(a, b) must_== 0f
    }

    "regard two input elements as the dissimilar if the types are different" in {
      val a = <input type="text" name="jill"/>
      val b = <input type="password" name="jill"/>
      compare(a, b) must_== 0f
    }
  }


}
