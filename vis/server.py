from flask import Flask
from flask import render_template
from flask import request
from flask import abort

import traceback

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

alt.data_transformers.enable('default', max_rows=1000000)

# TODO read in the data

app = Flask(__name__)


def test_chart():
    source = pd.DataFrame({
        'a': ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'],
        'b': [28, 55, 43, 91, 81, 53, 19, 87, 52]
    })
    return alt.Chart(source).mark_bar().encode(
        x='a',
        y='b'
    )


all_graph_specifications = {
    "Test Chart": test_chart
}


@app.get("/all_graphs")
def all_graphs():
    return sorted(list(all_graph_specifications.keys()))


@app.get("/graph/<name>")
def graph(name: str):
    chart_fn = all_graph_specifications.get(name)
    if chart_fn is None:
        abort(404)
    else:
        return chart_fn().to_json(0)


@app.post("/refresh")
def refresh():
    pass  # TODO
