from flask import Flask
from flask import render_template
from flask import request
from flask import abort
from flask import Response

import traceback
import json

from typing import *
import numpy as np  # type: ignore
import scipy  # type: ignore
import scipy.stats  # type: ignore
import altair as alt  # type: ignore
import pandas as pd  # type: ignore
import pprint
import textwrap
import copy

import itertools

from math import sqrt

from functools import reduce
from functools import lru_cache

from collections import namedtuple

alt.data_transformers.enable('default', max_rows=1000000)

# TODO read in the data


def read_data():
    global debates
    debates = pd.read_csv('save/official/summaries/debates.csv')


read_data()

app = Flask(__name__)


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
