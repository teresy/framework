package net.liftweb.http

import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification

object UpdateDOMGeneratedCases extends Specification with XmlMatchers {
  import UpdateDOMSpec._

  "UpdateDOM Generated Cases".title

  "UpdateDOM" should {
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
