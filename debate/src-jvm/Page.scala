package debate

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
        // We don't currently use the room name here, but probably want to later for easily linking
        // to a debate.
        div(id:="defaultRoomName", value := roomName),
        // This div is where the React app will go.
        div(id:="contents"),
        // the filenames here match the outputs of the Mill build artifacts from JS build steps,
        // which we're including in the resource path in the Mill build.
        script(src := s"$staticFilePrefix/jsdeps.js"), // from SimpleJSDepsBuild#aggregatedJSDeps
        script(src := s"$staticFilePrefix/out.js") // from fastOptJS / fullOptJS -- the JS 'main'
      )
    )
  }
}
