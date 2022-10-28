package livechat

import scalatags.Text.all._

object Page {
  def apply(roomName: String = "") = {
    html(
      head(
        meta(charset := "utf-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        link(
          rel := "stylesheet",
          href := "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
        ),
        scalatags.Text.tags2.title(makePageTitle(roomName))
      ),
      body(
        div(id:="defaultRoomName", value := roomName),
        div(id:="contents"),
        script(src := "file/jsdeps.js"),
        script(src := "file/out.js")
      )
    )
  }
}
