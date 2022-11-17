# Debate
Web interface for information-asymmetric debates.

## Usage

Install [Mill](https://com-lihaoyi.github.io/mill/mill/Intro_to_Mill.html).

You can run the server in one of two ways:
* For development, run with e.g. `mill -i debate.jvm.run --port 8080 --save save`.
* For production, run with e.g. `mill -i debate.prod.run --port 8080 --save save`.

This will host your server by HTTP on port 8080.
To run HTTPS, there is also an `--ssl` flag which has the server look for a `keystore.jks` and
`password` under `debate/resources`, but I normally run behind a proxy which takes care of this.
The difference between development and production is that production mode uses fully-optimized JS
compilation, which takes longer but produces a much smaller and faster-running JS file.

To run all unit tests, do `mill __.test`.

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

### Suggestions for using VSCode

- Install the Scala Metals extension.
  - Why? This gives you nice stuff like type-at-point.
  - Check out the button on the left-hand-side of vscode. It has a lot of useful stuff.
- Use `bloop` as the build server.
- Use `scalafix` somewhat-often. Check out e.g. `mill debate.jvm.fix`.
 - TODO make this run automatically
 - TODO add the todo -> github issue thing
- For a nice loop in case metals is slow, check out `mill -w -i debate.jvm.run  --port 8080 --save save`

### A Simple Loop

- Start the server
- Load the site and join a new debate as a facilitator.
  - You can change roles between the participants and the judge.
  - TODO can we make this easier- load an example debate?
  - TODO should the example be shared between the debaters?
- You can also see what observers see by clicking `Observer`

## Background

The code in this repository is written in Scala in functional style.

On Scala and FP:
* [Why Scala?](http://www.lihaoyi.com/post/FromFirstPrinciplesWhyScala.html) By Li Haoyi
* [What's Functional Programming All About?](https://www.lihaoyi.com/post/WhatsFunctionalProgrammingAllAbout.html) By Li Haoyi

Relevant libraries to reference:
* [cats](https://typelevel.org/cats/): Basic functional programming abstractions
* [cats-effect](https://typelevel.org/cats-effect/): Basic functional programming abstractions
* [monocle](https://www.optics.dev/Monocle/): Optics for accessing/transforming immutable data
* [scalajs-react](https://github.com/japgolly/scalajs-react): React facade for Scala
* [jjm](https://github.com/julianmichael/jjm): My personal library of utilities on top of these tools
* [munit](https://scalameta.org/munit/): Unit testing
