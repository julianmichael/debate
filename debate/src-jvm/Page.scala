package debate

import scalatags.Text.all._

object Page {
  def apply(
    jsLocation: String,
    roomName: String = "",
  ) = {
    html(
      head(
        meta(charset := "utf-8"),
        meta(
          name := "viewport",
          content := "width=device-width, initial-scale=1"
        ),
        link(
          rel := "stylesheet",
          href := "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"
        ),
        scalatags.Text.tags2.title(makePageTitle(roomName))
      ),
      body(
        // We don't currently use the room name here, but probably want to later for easily linking
        // to a debate.
        div(id := "defaultRoomName", value := roomName),
        // This div is where the React app will go.
        div(id := "contents"),
        // this is included as a workaround for a linking problem I (julianmichael) haven't figured out
        script(
          src := "https://cdn.jsdelivr.net/npm/js-cookie@3.0.1/dist/js.cookie.min.js"
        ),
        script(
          src := s"$staticFilePrefix/$jsLocation"
        ), // from devBundle / prodBundle -- the JS 'main'
        script(
          src := s"$staticFilePrefix/$jsLocation.map"
        ) // source map
      )
    )
  }
}
