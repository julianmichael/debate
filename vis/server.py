import sys
from flask import Flask
from flask import abort
from flask import Response

import datetime
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
    debates = debates[debates['Start time'] >
                      pd.to_datetime('10/02/23', format='%d/%m/%y')]
    debates['Final probability incorrect'] = (
        1 - debates['Final probability correct'])
    debates['End time'] = pd.to_datetime(
        debates['End time'], unit='ms')
    sessions = pd.read_csv(
        os.path.join(data_dir, 'official/summaries/sessions.csv')
    )
    turns = pd.read_csv(
        os.path.join(data_dir, 'official/summaries/turns.csv')
    )
    turns['Room start time'] = pd.to_datetime(
        turns['Room start time'], unit='ms')
    turns = turns[turns['Room start time'] >
                      pd.to_datetime('10/02/23', format='%d/%m/%y')]

    print("Debates:")
    print(debates.dtypes)
    print(debates.describe())
    print("Turns:")
    print(turns.dtypes)
    print(turns.describe())
    print("Sessions:")
    print(sessions.dtypes)
    print(sessions.describe())


read_data()

# MAIN RESULTS


def an_overview_of_counts():
    debates['Final probability correct (live + mean of offline)'] = debates.apply(
        lambda row: row['Final probability correct'] if row['Offline'] == False else row['Average offline probability correct'], axis=1)
    bins = [0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1]
    labels = ['0-10%', '10-20%', '20-30%', '30-40%', '40-50%', '50-60%', '60-70%', '70-80', '80-90%', '90-100%']
    debates['Final probability correct bins'] = pd.cut(debates['Final probability correct (live + mean of offline)'], 
                                                       bins=bins, labels=labels)
    counts_bar = alt.Chart(debates).mark_bar().encode(
        x=alt.X('count()',stack='zero'),
        y=alt.Y('Status:O', sort=alt.EncodingSortField(
            op='count', order='descending')),
        color=alt.Color('Final probability correct bins:O', scale=alt.Scale(scheme='redyellowblue')),
    
    ).facet(facet = 'Offline:O', columns=1)
    # counts_text = alt.Chart(debates).mark_text(dx=-15, dy=3).encode(
    #     x=alt.X('count()',stack='zero'),
    #     y=alt.Y('Status:O', sort=alt.EncodingSortField(
    #         op='count', order='descending')),
    #     color=alt.Color('Final probability correct bins:O', scale=alt.Scale(range=['black'])),
    #     text=alt.Text('count()')
    # )
    return counts_bar.properties(width=850, height=300)
    # return alt.layer(counts_bar, counts_text).resolve_scale(color='independent').properties(width=850, height=300)


# TRACK


def debater_pairings_by_role():
    return alt.Chart(debates).mark_rect().encode(
        x='Honest debater:O',
        y=alt.Y('Dishonest debater:O', scale=alt.Scale(reverse=True)),
        color='count():Q'
    )


def debater_pairings_by_person():  # TODO Instead of having them in separate items on the dropdown menu, hv multiple tweakers for the same viewed data?
    return


def final_probability_by_honest_and_dishonest_debater():
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
            field='Final probability incorrect',
            op='mean',
            order='descending'
        )
        ),
        y='mean(Final probability incorrect):Q'
    )
    dishonest_err = alt.Chart(debates).mark_rule().encode(
        x=alt.X('Dishonest debater:O', sort=alt.EncodingSortField(
            field='Final probability incorrect',
            op='mean',
            order='descending'
        )),
        y='ci0(Final probability incorrect)',
        y2='ci1(Final probability incorrect)',
        # y=alt.Y('Final probability correct:Q', scale=alt.Scale(zero=False))
    )
    # return alt.hconcat(honest, dishonest, )
    return (honest_bar + honest_err) | (dishonest_bar + dishonest_err)


def evidence_by_roundss():
    evidence_line = alt.Chart(turns[turns[['Start time'] >
                      pd.to_datetime('10/02/23', format='%d/%m/%y')]]).mark_area().encode(
        x='Num previous debating rounds:O',
        y='ci0(Quote length)',
        y2='ci1(Quote length)'
    ).facet(
        facet='Participant:N',
        columns=6
    ).properties(width=850)
    # evidence_err = alt.Chart(debates).mark_errorbar().encode(
    #     x='Num previous debating rounds:O',
    #     y='ymin:Q',
    #     y2='ymax:Q'
    # )
    return evidence_line  # + evidence_err


def evidence_by_rounds():
    evidence_line = alt.Chart(turns).mark_line().encode(
        x='Num previous debating rounds:O',
        y='mean(Quote length)',
        color='Participant:N'
    ).properties(width=850)
    # evidence_err = alt.Chart(debates).mark_errorbar().encode(
    #     x='Num previous debating rounds:O',
    #     y='ymin:Q',
    #     y2='ymax:Q'
    # )
    return evidence_line  # + evidence_err


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


def participant_by_past_workloads():
    source = sessions.merge(debates, how='left', on='Room name')
    source['Role'] = source['Role'].map(
        lambda x: 'Debater' if x.startswith('Debater') else x)
    source['End date'] = source['End time'] - pd.to_timedelta(6, unit='d')
    source_by_week = source.groupby(
        ['Participant', pd.Grouper(key='End date', freq='W-MON')]).sum().reset_index().sort_values('End date')

    return alt.Chart(source_by_week).mark_bar().encode(
        x=alt.X('count()'),
        y=alt.Y('Participant:O', sort=alt.EncodingSortField(
            op='count', order='descending')),
        color=alt.Color('End date:O'),
        column=alt.Column('End date:O')
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


def probability_correct_by_num_rounds():
    return alt.Chart(debates).mark_circle(size=60).encode(
        x='Num rounds:O',
        y='Final probability correct:Q',
        # color='Judge:N',
        tooltip=['Room name']
    ).properties(width=750)


def probability_correct_over_time():
    return alt.Chart(debates).mark_bar().encode(
        x='yearmonthdate(End time):T',
        y='mean(Final probability correct):Q'
    ).properties(width=750)


def debates_completed_per_week():
    # debates['End date'] = debates['End time'] - pd.to_timedelta(6, unit='d')

    debates['End week'] = debates['End time'].apply(
        lambda x: x.week
        # lambda x: f'{(x.dt.week).strftime("%b %d")} - {x.strftime("%B %d")}'
    )

    def convert_time(x):
        start_date = datetime.datetime.strptime(
            f'{x.year}-{x.week}-1', "%Y-%W-%w"
        )
        end_date = start_date + pd.Timedelta(days=6)
        return f'{start_date.strftime("%b %d")} - {end_date.strftime("%B %d")}'
    debates['End week label'] = debates['End time'].apply(
        convert_time
        # lambda x: datetime.datetime.strptime(
        #     f'{x.year}-{x.week}-1', "%Y-%W-%w"
        # ).strftime("%b %d") + ' - ' + datetime.datetime.strptime(
        #     f'{(x + pd.Timedelta(days=6)).year}-{(x + pd.Timedelta(days=6)).week}-0', "%Y-%W-%w"
        # ).strftime("%B %d")
        # lambda x: f'{x.year}- {(x.week).strftime("%b %d")} - {x.strftime("%B %d")}'
    )
    # add a column to debates with a human-readable date range from End week (week number) using pandas
    # debates['End week label'] = debates['End time'].apply(
    #     lambda x: f'{(x - pd.to_timedelta(6, unit="d")).strftime("%b %d")} - {x.strftime("%b %d")}'
    # )
    all_bar = alt.Chart(debates[debates["Is over"] == True]).mark_bar().encode(
        x=alt.X('End week label:O', sort=alt.EncodingSortField(
            field='End week', order='ascending')),
        y='count():Q'
    ).properties(width=750)

    # all_line = alt.Chart(debates[debates["Is over"] == True].melt(id_vars=['Room name', 'End week label', 'End week'], value_vars=('Honest debater', 'Dishonest debater'), value_name='Debater')).mark_line().encode(
    #     x=alt.X('End week label:O', sort=alt.EncodingSortField(
    #         field='End week', order='ascending')),
    #     y='count():Q',
    #     color='Debater:N'
    # ).properties(width=750)

    return all_bar # + all_line


# Keys must be valid URL paths. I'm not URL-encoding them.
# Underscores will be displayed as spaces in the debate webapp analytics pane.
all_graph_specifications = {
    "Main_results:_An_overview_of_counts": an_overview_of_counts,
    # "Main_results:_Offline_vs_live_debates": offline_vs_live_debates,
    "Main_results:_Debates_right_over_time": probability_correct_over_time, ##
    "Results:_Evidence_by_rounds": evidence_by_rounds,
    "Results:_Probability_correct_by_num_rounds": probability_correct_by_num_rounds,
    "Results:_Final_probability_by_debaters": final_probability_by_honest_and_dishonest_debater,
    "Track:_Debates_completed_per_week": debates_completed_per_week,
    "Track:_Participant_by_current_workload": participant_by_current_workload,
    # "Track:_Participant_by_past_workload": participant_by_past_workload,
    # "Track:_Turns_to_complete_by_participant": turns_to_complete_by_participant,
    "Track:_Debater_pairings_by_role": debater_pairings_by_role,
    # "Track:_Debater_pairings_by_person": debater_pairings_by_person,
    "Track:_Judge_pairings": judge_pairings,
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
