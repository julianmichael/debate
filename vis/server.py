import sys
from flask import Flask
from flask import abort
from flask import Response

import json

from typing import *
import numpy as np  # type: ignore
import altair as alt  # type: ignore
import pandas as pd  # type: ignore

from functools import reduce
from functools import lru_cache

from collections import namedtuple

import os

app = Flask(__name__)
app.config.from_prefixed_env()

alt.data_transformers.enable('default', max_rows=1000000)


def read_data():
    global debates
    debates = pd.read_csv(
        os.path.join(app.config['DATA_DIR'], 'official/summaries/debates.csv')
    )


read_data()


def debater_pairings():
    return alt.Chart(debates).mark_rect().encode(
        x='Honest debater:O',
        y=alt.Y('Dishonest debater:O', scale=alt.Scale(reverse=True)),
        color='count():Q'
    )


all_graph_specifications = {
    "debater_pairings": debater_pairings


}


@ app.get("/all_graphs")
def all_graphs():
    result = sorted(list(all_graph_specifications.keys()))
    return Response(json.dumps(result),  mimetype='application/json')


@ app.get("/graph/<name>")
def graph(name: str):
    chart_fn = all_graph_specifications.get(name)
    if chart_fn is None:
        abort(404)
    else:
        return chart_fn().to_json(0)


@ app.post("/refresh")
def refresh():
    read_data()
    return {}
