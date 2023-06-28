# Debate
Web interface for information-asymmetric debates.

## Usage

### Requirements
* Install [Mill](https://com-lihaoyi.github.io/mill/mill/Intro_to_Mill.html).

For the analytics server, you'll also need Python 3.
I recommend using it in a virtual environment. To get set up, run:
```bash
python -m venv env
source env/bin/activate
pip install -r requirements.txt
```

### Running the webapp

For development, run
```bash
mill -j 0 debate.dev.serve
```
in the base directory of this repository. (`-j 0` will parallelize and speed up compilation.)

You can also pass in flags at runtime:
* `--port`: the port to host the server at. (default: 8080)
* `--analytics-port`: the port the analytics server will be hosted at. (default: 8081)
* `--save`: the directory to save the server state at. (default: `save`)
* `--help`: print command info instead of running the server.

To run HTTPS, there is also an `--ssl` flag which has the server look for a `keystore.jks` and
`password` under `debate/resources`, but I normally run behind a proxy which takes care of this.
The difference between development and production is that production mode uses fully-optimized JS
compilation, which takes longer but produces a much smaller and faster-running JS file.

To run unit tests, use `mill debate.jvm.test`. (JS tests aren't working at the moment; see #76.)

### Running the model debate server

To run debates with GPT-4 as a participant, you will need to have the packages in `model-debate/requirements.txt` installed. To start the server (which formats and processes POST requests sent from the main Scala webapp, before sending the debate transcript to GPT-4), navigate to the subdirectory `model-debate` and run 
```bash
uvicorn app:app
```

You can also pass in the following flags:
* `---port`: the port to host the server at.
* `---reload`: if you're doing development on the server, and want automatic reloading.

### Running the analytics server

There's a python server to produce the visualizations in the Analytics tab of the interface.
After activating your virtual environment and installing dependencies as described above, run
To start this, run:
```bash
FLASK_APP=vis/server.py python -m flask run --port 8081
```
and the analytics pane should work in the debate webapp assuming you ran that also without changing
any command line arguments). The `--port` argument (default: 5000) should match the `--analytics-port` argument
from the main webapp. If you are using a save directory other than `save`, you can pass it in
using the `DATA_DIR` environment variable, e.g.:
```bash
FLASK_APP=vis/server.py DATA_DIR=scratch/save-server python -m flask run --port 8081
```
The value you pass in for `DATA_DIR` should match that of `--save` for the main webapp.

## Contents

* `build.sc`: Mill build file.
* `debate/src{,-jvm,-js}`: Scala source for all platforms.
* `debate/test/src`: Tests.
* `vis/`: Python code for the analytics visualization server.
* `scripts/`: Some python scripts for working with QuALITY stories.

## Development

JVM entry point (debate webapp server):
* [Serve.scala](debate/src-jvm/Serve.scala)

JS entry point (debate webapp client):
* [App.scala](debate/src-js/App.scala)

Python entry point (analytics visualization server):
* [server.py](vis/server.py)

### Live Testing

After starting up the server, go to the page and open the Admin tab.
There you can add/remove debater profiles, create debates, etc.

If you change the JS source only, then you can run `mill debate.js.fastestOpt` and hard refresh the
page when it's done to load the changes. If you change the JVM or shared source as well, then
you'll need to restart the server (i.e., interrupt and re-run `mill debate.dev.serve`).

### Suggestions for using VSCode

- Install the Scala Metals extension.
  - Why? This gives you nice stuff like type-at-point, completion with types, etc.
  - Check out the button on the left-hand-side of vscode. It has a lot of useful stuff.
- Use `bloop` as the build server.
- Use `scalafix` somewhat-often. Check out e.g. `mill debate.jvm.fix`.

### Setting up a powerful instance for fast builds

 Jess Smith did the following to speed up compilation and linking. This was, in his view, surprisingly cheap for the productivity gains. (Julian has not had problems with Metals in VSCode on his M2 Air, but `fastestOpt` sometimes takes ~10s, and YMMV.)

 1. Set up a powerful instance on google cloud platform. (*be sure to shut this down when you're done using it, else you can be billed a lot*) Jess uses a c2d-highcpu-8 instance in us-central1-a using debian-11-bullseye-v20221206. (As of 11 Dec 2022.)
 2. Install homebrew. (This was, in his experience, the least painful way to set up mill.)
 1. Install mill using homebrew.
 4. Use vscode remote ssh to connect to the instance.
 5. Use ssh forwarding to connect to the instance from your local machine. (This lets you use e.g. `localhost:8080` to connect to `server:8080`).
 6. Reminder again to shut down the instance when you're done using it. :)

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
