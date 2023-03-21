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

from altair import datum

app = Flask(__name__)

alt.data_transformers.enable('default', max_rows=1000000)

data_dir = os.environ.get('DATA_DIR', default='save')


def read_data():
    global debates
    global sessions
    global turns
    debates = pd.read_csv(
        os.path.join(data_dir, 'official/summaries/debates.csv')
    )
    debates['Start time'] = pd.to_datetime(
        debates['Start time'], unit='ms')
    debates['Final_probability_incorrect'] = (
        1 - debates['Final probability correct'])
    debates['End time'] = pd.to_datetime(
        debates['End time'], unit='ms')
    sessions = pd.read_csv(
        os.path.join(data_dir, 'official/summaries/sessions.csv')
    )
    turns = pd.read_csv(
        os.path.join(data_dir, 'official/summaries/turns.csv')
    )

    print("Debates:")
    print(debates.dtypes)
    print(debates)
    print("Sessions:")
    print(sessions.dtypes)
    print(sessions)


read_data()


def debater_pairings_by_role():
    return alt.Chart(debates).mark_rect().encode(
        x='Honest debater:O',
        y=alt.Y('Dishonest debater:O', scale=alt.Scale(reverse=True)),
        color='count():Q'
    )


def debater_pairings_by_person():  # TODO Instead of having them in separate items on the dropdown menu, hv multiple tweakers for the same viewed data?
    return


def honest_and_dishonest_debater_by_final_probability():
    honest_bar = alt.Chart(debates).mark_bar().encode(
        x=alt.X('Honest debater:O', sort=alt.EncodingSortField(
            field='Final probability correct',
            op='mean',
            order='descending'
        )
        ),
        y='mean(Final probability correct):Q'
    )
    honest_err = alt.Chart(debates).mark_rule().encode(
        x=alt.X('Honest debater:O', sort=alt.EncodingSortField(
            field='Final probability correct',
            op='mean',
            order='descending'
        )),
        y='ci0(Final probability correct)',
        y2='ci1(Final probability correct)',
        # y=alt.Y('Final probability correct:Q', scale=alt.Scale(zero=False))
    )
    dishonest_bar = alt.Chart(debates).mark_bar().encode(
        x=alt.X('Dishonest debater:O', sort=alt.EncodingSortField(
            field='Final probability correct',
            op='mean',
            order='descending'
        )
        ),
        y='mean(Final probability correct):Q'
    )
    dishonest_err = alt.Chart(debates).mark_rule().encode(
        x=alt.X('Dishonest debater:O', sort=alt.EncodingSortField(
            field='Final probability correct',
            op='mean',
            order='descending'
        )),
        y='ci0(Final probability correct)',
        y2='ci1(Final probability correct)',
        # y=alt.Y('Final probability correct:Q', scale=alt.Scale(zero=False))
    )
    # return alt.hconcat(honest, dishonest, )
    return (honest_bar + honest_err) | (dishonest_bar + dishonest_err)


def evidence_by_rounds():
    evidence_line = alt.Chart(turns).mark_line().encode(
        x='Debating Rounds So Far:O',
        y='mean(Quote length)'
    )
    evidence_err = alt.Chart(debates).mark_errorbar().encode(
        x='Debating Rounds So Far:O',
        y='ymin:Q',
        y2='ymax:Q'
    )
    return evidence_line + evidence_err


def participant_by_current_workload():
    # debates.set_index('Room name')
    source = sessions.merge(debates, how='left', on='Room name')
    source['Role'] = source['Role'].map(
        lambda x: 'Debater' if x.startswith('Debater') else x)

    return alt.Chart(source).mark_bar().encode(
        x=alt.X('count()'),
        y=alt.Y('Participant:O', sort=alt.EncodingSortField(
            op='count', order='descending')),
        color=alt.Color('Role:O'),
        column=alt.Column('Role:O')
    ).transform_filter(
        datum['Status'] != 'complete'
    ).properties(width=200)


def participant_by_past_workload():
    source = sessions.merge(debates, how='left', on='Room name')
    return alt.Chart(source).mark_bar().encode(
        x=alt.X('count()'),
        y=alt.Y('Participant:O', sort=alt.EncodingSortField(
            op='count', order='descending')),
        color=alt.Color('Role:O'),
        column=alt.Column('Role:O')
    ).transform_filter(
        datum['Status'] == 'complete'
    ).properties(width=200)


def judge_by_final_probability():
    return alt.Chart(debates).mark_bar().encode(
        x=alt.X('Live judge:O', sort='-y'),
        y='Average_final_probability:Q'
    ).transform_aggregate(
        Average_final_probability='mean(Final probability correct)',
        groupby=['Judge']
    )


def judge_pairings():
    return alt.Chart(debates.melt(id_vars='Live judge', value_vars=('Honest debater', 'Dishonest debater'), value_name='Debater')).mark_rect().encode(
        x='Live judge:O',
        y=alt.Y('Debater:O', scale=alt.Scale(reverse=True)),
        color='count():Q'
    )


def probability_correct_vs_num_rounds():
    return alt.Chart(debates).mark_circle(size=60).encode(
        x='Number of rounds:O',
        y='Final probability correct:Q',
        # color='Judge:N',
        tooltip=['Room name']
    ).properties(width=750)


def probability_correct_over_time():
    return alt.Chart(debates).mark_bar().encode(
        x='yearmonthdate(Start time):T',
        y='mean(Final probability correct):Q'
    ).properties(width=750)


def debates_completed_per_week():
    debates['End date'] = debates['End time'] - pd.to_timedelta(7, unit='d')
    print(debates['End date'])
    debates_by_week = debates.groupby(
        ['Room name', pd.Grouper(key='End date', freq='W-MON')]).sum().reset_index().sort_values('End date')
    print(debates_by_week)
    print(debates_by_week.dtypes)
    return alt.Chart(debates_by_week[debates_by_week["Is over"] == True]).mark_bar().encode(
        x='End date:T',
        y='count(Room name):Q'
    )


# Keys must be valid URL paths. I'm not URL-encoding them.
# Underscores will be displayed as spaces in the debate webapp analytics pane.
all_graph_specifications = {
    "Debater_pairings_by_role": debater_pairings_by_role,
    # "Debater_pairings_by_person": debater_pairings_by_person,
    "Debaters_by_final_probability": honest_and_dishonest_debater_by_final_probability,
    "Judge_pairings": judge_pairings,
    "Probability_correct_vs_num_rounds": probability_correct_vs_num_rounds,
    "Probability_correct_over_time": probability_correct_over_time,
    "Participant_by_current_workload": participant_by_current_workload,
    # "Participant_by_past_workload": participant_by_past_workload,
    "Evidence_by_rounds": evidence_by_rounds,
    "Debates_completed_per_week": debates_completed_per_week
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
