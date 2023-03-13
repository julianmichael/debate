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

alt.data_transformers.enable('default', max_rows=1000000)

data_dir = os.environ.get('DATA_DIR', default='save')


def read_data():
    global debates
    debates = pd.read_csv(
        os.path.join(data_dir, 'official/summaries/debates.csv')
    )


read_data()


def debater_pairings_by_role():
    return alt.Chart(debates).mark_rect().encode(
        x='Honest debater:O',
        y=alt.Y('Dishonest debater:O', scale=alt.Scale(reverse=True)),
        color='count():Q'
    )


def debater_pairings_by_person():  # TODO Instead of having them in separate items on the dropdown menu, hv multiple tweakers for the same viewed data?
    return


def honest_debater_by_final_probability():  # esp. for here...
    return alt.Chart(debates).mark_bar().encode(
        x=alt.X('Honest debater:O', sort='-y'),
        y='Average_probability:Q'
    ).transform_aggregate(
        Average_probability='mean(Final probability correct)',
        groupby=['Honest debater']
    )


def dishonest_debater_by_final_probability():
    return alt.Chart(debates).mark_bar().encode(
        x=alt.X('Dishonest debater:O', sort='-y'),
        y='Average_probability:Q'
    ).transform_aggregate(
        Average_probability='mean(Final probability correct)',
        groupby=['Dishonest debater']
    )


def judge_pairings():  # might not be the Altair way
    return alt.Chart(debates.melt(id_vars='Judge', value_vars=('Honest debater', 'Dishonest debater'), value_name='Debater')).mark_rect().encode(
        x='Judge:O',
        y=alt.Y('Debater:O', scale=alt.Scale(reverse=True)),
        color='count():Q'
    )


def probability_correct_vs_num_rounds():
    return alt.Chart(debates).mark_circle(size=60).encode(
        x='Rounds:Q',  # widens the graph, probably not best way
        y='Final probability correct:Q',
        # color='Judge:N',
        tooltip=['Room name']
    ).interactive()


# Keys must be valid URL paths. I'm not URL-encoding them.
# Underscores will be displayed as spaces in the debate webapp analytics pane.
all_graph_specifications = {
    "Debater_pairings_by_role": debater_pairings_by_role,
    # "Debater_pairings_by_person": debater_pairings_by_person,
    "Honest_debater_by_final_probability": honest_debater_by_final_probability,
    "Dishonest_debater_by_final_probability": dishonest_debater_by_final_probability,
    "Judge_pairings": judge_pairings,
    "Probability_correct_vs_num_rounds": probability_correct_vs_num_rounds
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
