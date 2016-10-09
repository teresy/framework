package net.liftweb.http

import java.io.{File, FileWriter}

import com.gargoylesoftware.htmlunit.BrowserVersion._
import com.gargoylesoftware.htmlunit.WebClient
import net.liftweb.http.js.JE
import net.liftweb.json.Extraction
import net.liftweb.util.VDom._
import net.liftweb.util.{Html5, VDom}
import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification
import net.sourceforge.htmlunit.corejs.javascript.{ScriptableObject, Function => JsFunction}
import com.gargoylesoftware.htmlunit.html.{DomElement, DomNode, HtmlElement, HtmlPage}
import net.liftweb.util.VDom.VDomHelpers._

import scala.xml._
import scala.collection.JavaConverters._

object UpdateDOMSpec extends Specification with XmlMatchers {
  implicit val formats = VDom.formats

  "UpdateDOM Spec".title

  def rtAndCompare(before:Node, after:Node) = {
    import VDomHelpers._
    val result = withoutWhitespace(roundTrip(before, after))

    result must beEqualTo(withoutWhitespace(after))
  }

  def roundTrip(before:Node, after:Node):Node = {
    val lift_js = this.getClass.getClassLoader.getResource("toserve/lift.js")
    val jq_js   = this.getClass.getClassLoader.getResource("toserve/jquery-1.4.4.js")
    def html(body:Node) =
      <html>
        <head>
          <meta charset="UTF-8"/>
          <title>Home</title>
          <script type="application/javascript" language="javascript" src={jq_js.toURI.toASCIIString}></script>
          <script type="application/javascript" language="javascript" src={lift_js.toURI.toASCIIString}></script>
        </head>
        {body}
      </html>

    def toXml(e:DomNode):Node = {
      val attrs:MetaData = (0 until e.getAttributes.getLength).foldLeft(Null:MetaData) {
        case (acc, i) =>
          val attr = e.getAttributes.item(i)
          new UnprefixedAttribute(attr.getNodeName, Text(attr.getNodeValue), acc)
      }
      val children = e.getChildren.asScala.map(toXml).toSeq
      if(e.getNodeName == "#text") Text(e.getTextContent)
      else Elem(null, e.getNodeName, attrs, TopScope, true, children:_*)
    }

    val diff = VDom.diff(0, before, after, List())
    val js = JE.Call("lift.updateBody", Extraction.decompose(diff)).toJsCmd

    val file = File.createTempFile("test", "html")
    try {
      val w = new FileWriter(file)
      w.write("<!DOCTYPE html>")
      Html5.write(html(before), w, true, true)
      w.close()

      val client = new WebClient(CHROME)
      val options = client.getOptions()
      options.setHomePage(WebClient.URL_ABOUT_BLANK.toString())
      options.setJavaScriptEnabled(true)

      client.getPage(file.toURI.toURL)
      val window = client.getCurrentWindow().getTopWindow
      val page:HtmlPage = window.getEnclosedPage().asInstanceOf[HtmlPage] // asInstanceOf because ... java...

      def exec(js:String):String = {
        val toRun = "function() {\n"+js+"\n};"
        val result = page.executeJavaScript(toRun)
        val func:JsFunction = result.getJavaScriptResult().asInstanceOf[JsFunction]

        val exeResult = page.executeJavaScriptFunctionIfPossible(
          func,
          window.getScriptableObject(),
          Array.empty,
          page.getDocumentElement()
        )

        exeResult.getJavaScriptResult.toString
      }

      exec(js)

      toXml(page.getBody)
    } finally {
      file.delete()
    }
  }

  "UpdateDOM" should {
    "append an element" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
              <li>Message 3</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "append two elements" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
              <li>Message 3</li>
              <li>Message 4</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "insert an element" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 3</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "insert an element identical to a sibling" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }


    "insert two consecutive elements" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 3</li>
              <li>Message 2</li>
              <li>Message 1</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "remove two consecutive elements" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
              <li>Message 3</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 3</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "remove an element identical to a sibling" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "add an element and remove an element" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
              <li>Message 3</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 3</li>
              <li>Message 4</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "add an element before and remove an element" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
              <li>Message 3</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 4</li>
              <li>Message 1</li>
              <li>Message 3</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "add two elements and remove two elements" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
              <li>Message 3</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 3</li>
              <li>Message 4</li>
              <li>Message 5</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "add two elements before and remove two elements" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
              <li>Message 3</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 4</li>
              <li>Message 5</li>
              <li>Message 3</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "insert an element with attributes" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <ul>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <ul>
              <li>Message 2</li>
              <li class="chat-message clearable">Message 3</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "add elements in two locations of the tree" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <ul>
              <li>Message 1</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "find reordered elements" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
              <li>Message 3</li>
              <li>Message 4</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 2</li>
              <li>Message 4</li>
              <li>Message 3</li>
              <li>Message 1</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "find one pair of swapped elements" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 2</li>
              <li>Message 1</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "find more swapped reordered elements" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
              <li>Message 3</li>
              <li>Message 4</li>
              <li>Message 5</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 2</li>
              <li>Message 1</li>
              <li>Message 3</li>
              <li>Message 5</li>
              <li>Message 4</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "find added and swapped elements" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
              <li>Message 3</li>
              <li>Message 4</li>
              <li>Message 5</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 2</li>
              <li>Message 1</li>
              <li>Message 3</li>
              <li>Message 4</li>
              <li>Message 5</li>
              <li>Message 6</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "find deleted and swapped elements" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
              <li>Message 3</li>
              <li>Message 4</li>
              <li>Message 5</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 2</li>
              <li>Message 1</li>
              <li>Message 3</li>
              <li>Message 4</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "find added, deleted, and swapped elements" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
              <li>Message 3</li>
              <li>Message 4</li>
              <li>Message 5</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 2</li>
              <li>Message 1</li>
              <li>Message 3</li>
              <li>Message 4</li>
              <li>Message 6</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "update attributes which have been changed" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul class="bold">
              <li>Message 1</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul class="italics">
              <li>Message 1</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "find attributes which have been added" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul class="italics">
              <li>Message 1</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "find attributes which have been removed" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul class="bold">
              <li>Message 1</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "delete children text nodes when the parent attributes match" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li class="chat-message">Message 1</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li class="chat-message"/>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "add text only nodes" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <h2>Welcome to chat</h2>
            <span>Say something!</span>
            <ul>
              <li class="chat-message">Message 1</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <h2>Welcome to chat</h2>
            TEXT
            <span>Say something!</span>
            <ul>
              <li class="chat-message">Message 1</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "handle children which occur after a text node" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            TEXT
            <ul>
              <li class="chat-message">Message 1</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            TEXT
            <ul>
              <li class="chat-message">Message 1</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "prop failed test" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <ul>
              <li class="chat-message">Message 1</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <h2>Welcome to chat</h2>
            <ul>
              <li class="chat-message">Message 1</li>
              <li>Message 2</li>
              <li>Message 3</li>
            </ul>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "prop failed test2" in {
      val before =
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
        </body>

      val after =
        <body data-lift-content-id="main">
          <div id="main" data-lift="surround?with=default;at=content">
            <h2>Welcome to chat</h2>
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
                <li class="chat-message clearable">Message 3<br></br></li>
              </ul>
            </div>
          </div>
        </body>


      rtAndCompare(before, after)
      }

    "prop failed test3" in {
      val before =
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
        </body>

      val after =
        <body data-lift-content-id="main">
          <div id="main" data-lift="surround?with=default;at=content">
            <span/>
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
        </body>


      rtAndCompare(before, after)
    }

    "prop failed test4" in {
      val before =
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
        </body>

      val after =
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
                <li class="chat-message clearable">Message 3</li>
              </ul>
            </div>
          </div>
        </body>


      rtAndCompare(before, after)
    }

    "prop test failed 5" in {
      val before =
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
        </body>

      val after =
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
                <li class="chat-message clearable"/>
                <li class="chat-message clearable">Message 3</li>
                <p></p>
              </ul>
            </div>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "prop test failed 6" in {
      val before =
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
        </body>

      val after =
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
            <p></p>
            <div>
              <ul data-lift="Chat.messages">
                <li class="chat-message">Message 1</li>
                <li class="chat-message clearable">Message 2</li>
                <p></p>
                <li class="chat-message clearable">Message 3</li>
              </ul>
            </div>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "prop test failed 7" in {
      val before =
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
        </body>

      val after =
        <body data-lift-content-id="main">
          <div id="main" data-lift="surround?with=default;at=content">
            <h2>Welcome to chat</h2>
            <hr></hr>
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
          bGkzebhg
        </body>

      rtAndCompare(before, after)
    }

    "prop test failed 8" in {
      val before =
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
        </body>

      val after =
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
                <p></p>
                <li class="chat-message clearable">Message 2<br></br></li>
                <li class="chat-message clearable">Message 3</li>
              </ul>
            </div>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "prop test failed 9" in {
      val before =
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
        </body>

      val after =
        <body data-lift-content-id="main">
          <div id="main" data-lift="surround?with=default;at=content">
            <h2>Welcome to chat</h2>
            <span>Say something!</span>
            gzimolgHdCppspdn
            <form method="post" data-lift="form.ajax">
              <div data-lift="Chat.submit">
                <input type="text" id="chat-in" name="in"/>
                <input type="submit" value="Submit"/>
              </div>
            </form>
            <div>
              <ul data-lift="Chat.messages">
                <li class="chat-message">Message 1</li>
                <li class="chat-message clearable">Message 3</li>
              </ul>
            </div>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "prop test failed 10" in {
      val before =
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
        </body>

      val after =
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
            LorjnHpvakindyfhx
            <hr></hr>
            <div>
              <ul data-lift="Chat.messages">
                <li class="chat-message">Message 1</li>
                <li class="chat-message clearable">Message 2</li>
                <li class="chat-message clearable">Message 3</li>
              </ul>
            </div>
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "prop test failed 11" in {
      val before =
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
        </body>

      val after =
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
                <hr></hr>
              </ul>
            </div>
            gXbtgPndop
          </div>
        </body>

      rtAndCompare(before, after)
    }

    "prop test failed 12" in {
      val before =
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
        </body>

      val after =
        <body data-lift-content-id="main">
          <div id="main" data-lift="surround?with=default;at=content">
            <h2>Welcome to chat</h2>
            <span/>
            <form method="post" data-lift="form.ajax">
              <div data-lift="Chat.submit">
                <input type="text" id="chat-in" name="in"/>
                <input type="submit" value="Submit"/>
              </div>
            </form>
            iyorhsrwFx
            <div>
              <ul data-lift="Chat.messages">
                <li class="chat-message">Message 1</li>
                <li class="chat-message clearable">Message 2</li>
                <li class="chat-message clearable">Message 3</li>
              </ul>
            </div>
          </div>
        </body>


      rtAndCompare(before, after)
    }

    "prop test failed 13" in { // edge case caught by prop test, happens very rarely
      val before =
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
        </body>

      val after =
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
                <li class="chat-message clearable">Message 2<hr></hr></li>
                <li class="chat-message clearable">Message 3</li>
              </ul>
            </div>
          </div>
        </body>

      rtAndCompare(before, after)
    }.pendingUntilFixed
  }
}
