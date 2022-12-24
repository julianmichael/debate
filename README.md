# Debate
Web interface for information-asymmetric debates.

## Usage

### Requirements
* Install [Mill](https://com-lihaoyi.github.io/mill/mill/Intro_to_Mill.html).
* Install [npm](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm).

### Running

For development, you must run 3 commands in separate terminals in the root directory of the project:
 - `mill --no-server -j 8 -w debate.js.publcDev` to start incremental compilation of the frontend.
The `-w` flag means that it will watch for changes and recompile automatically.
 - `mill --no-server -j 8 -w debate.jvm [args]` to start incremental compilation of the backend
and run the backend server on port 8080.
 - `npm run dev` to start the Vite with live reloading.

For production, you can start the backend with the same command as above, but without the `-w` flag.
The frontend is built with `mill debate.js.publicProd` and then served with any static file server.

All of the relevant server state under is saved under `save/` by default.
To run HTTPS, there is also an `--ssl` flag which has the server look for a `keystore.jks` and
`password` under `debate/resources`, but I normally run behind a proxy which takes care of this.
The difference between development and production is that production mode uses fully-optimized JS
compilation, which takes longer but produces a much smaller and faster-running JS file.

To run all unit tests, use `mill __.test`.

## Contents

* `build.sc`, `build-scripts/`: Build files.
* `{shared,js,jvm}`: Source code and resources for all platforms.
* `print_story.py`, `requirements.txt`: A quick script for printing the text of QuALITY stories from
  their HTML.
* `package.json`: JavaScript dependencies.

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

## Speeding up your build

smithjessk observed that using `mill -j 0` sped up his builds a lot. (~33% for `mill debate._.compile`).
(This might use more memory though).

## In case a default profile isn't set up

You can add profiles with the **~secret admin controls~** which you can access by using the developer tools to change the `Styles-adminOnly` and disable `display: none`. You can find the element if you just select the profile dropdown and then scan the elements that come after it.
