# Debate
Web interface for information-asymmetric debates.

## Usage

* Install [Mill](https://com-lihaoyi.github.io/mill/mill/Intro_to_Mill.html).
* Run with e.g. `mill -i debate.jvm.run --port 8080 --save save`.

This will host your server by HTTP on port 8080.
To run HTTPS, there is also an `--ssl` flag which has the server look for a `keystore.jks` and
`password` under `debate/resources`, but I normally run behind a proxy which takes care of this.

## Contents

* `build.sc`, `build-scripts/`: Build files.
* `debate/{src,src-js,src-jvm}`: Source code (cross-platform, JS only, and JVM only, respectively).
* `print_story.py`, `requirements.txt`: A quick script for printing the text of QuALITY stories from
  their HTML.

## Development

Java entry points:
* [Serve.scala](debate/src-jvm/Serve.scala)

JS entry point:
* [App.scala](debate/src-js/App.scala)
