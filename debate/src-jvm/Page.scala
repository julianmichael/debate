package debate

import scalatags.Text.all._

object Page {
  def apply(jsLocation: String, jsDepsLocation: String, roomName: String = "") = html(
    head(
      meta(charset := "utf-8"),
      meta(name    := "viewport", content := "width=device-width, initial-scale=1"),
      link(
        rel  := "stylesheet",
        href := "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"
      ),
      link(
        rel  := "stylesheet",
        href := "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.2/font/bootstrap-icons.css"
      ),
      link(
        media := "all",
        rel   := "stylesheet",
        href  := "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
      ),
      scalatags.Text.tags2.title(makePageTitle(roomName))
    ),
    body(
      // We don't currently use the room name here, but probably want to later for easily linking
      // to a debate.
      div(id := "defaultRoomName", value := roomName),
      // This div is where the React app will go.
      div(id := appDivId),
      // js deps
      script(src := s"$staticFilePrefix/$jsDepsLocation"),
      // js main
      script(src := s"$staticFilePrefix/$jsLocation")
    )
  )
}
