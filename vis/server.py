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

alt.data_transformers.enable("default", max_rows=1000000)

data_dir = os.environ.get("DATA_DIR", default="save")

# set graphic parameters
correctColor = "green"
incorrectColor = "crimson"
nullColor = "grey"
onlineColor = "orange"
offlineColor = "blue"
aggColor = "black"
fullWidth = 850
fullHeight = 400


def read_data():
    global debates
    global sessions
    global turns
    debates = pd.read_csv(os.path.join(data_dir, "official/summaries/debates.csv"))
    debates["Start time"] = pd.to_datetime(debates["Start time"], unit="ms")
    debates = debates[
        debates["Start time"] > pd.to_datetime("10/02/23", format="%d/%m/%y")
    ]
    debates["Final probability incorrect"] = 1 - debates["Final probability correct"]
    debates["End time"] = pd.to_datetime(debates["End time"], unit="ms")
    sessions = pd.read_csv(os.path.join(data_dir, "official/summaries/sessions.csv"))
    turns = pd.read_csv(os.path.join(data_dir, "official/summaries/turns.csv"))
    turns["Room start time"] = pd.to_datetime(turns["Room start time"], unit="ms")
    turns = turns[
        turns["Room start time"] > pd.to_datetime("10/02/23", format="%d/%m/%y")
    ]

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

# # Organizing graphs a bit since there are more now
# Main results: important for the paper/us to easily see
# Results: possibly in the paper
# Track: for us to make sure the system/debaters is/are working, possibly in Appendix or not in the paper
# # A bit silly, but for things to show up in the order of a possible paper outline,
# # first letter of sections or graph names take to account server's alphabetical dropdown for now


# MAIN RESULTS
# 1. An overview of counts | 2. ??? not sure yet what our main results are, considering setup comparison (off/live)
# might prove less interesting than we thought, and we haven't formally analyzed effect of other variations

# 1. An overview of counts
# The following graph is what would probably be a 2 x 2 table or a sentence or two in the paper
# ex "We did 75 offline debates and 75 live debates.
# The offline debates were 50% correct, and the live debates were 60% correct." (Copilot, 2023)


def an_overview_of_counts():  # TODO: un-average offline
    debates["Final probability correct (live and mean of offline)"] = debates.apply(
        lambda row: row["Final probability correct"]
        if row["Is offline"] == False
        else row["Average offline probability correct"],
        axis=1,
    )
    bins = [0, 0.491, 0.509, 1]
    labels = ["0-49%", "0.5", "51-100%"]
    debates["Final probability correct bins"] = pd.cut(
        debates["Final probability correct (live and mean of offline)"],
        bins=bins,
        labels=labels,
    )
    counts_bar = (
        alt.Chart(debates)
        .mark_bar()
        .encode(
            x=alt.X("count()", stack="zero", title="Number of debates"),
            y=alt.Y("Status:O", title=None),
            color=alt.Color(
                "Final probability correct bins:O",
                sort="descending",
                scale=alt.Scale(range=[correctColor, incorrectColor, nullColor]),
            ),
            row=alt.Row(
                "Is offline:N",
                header=alt.Header(
                    title="Debates categorized by correctness, setup, and status",
                    titleFontSize=18,
                    titleFontWeight="bold",
                    titleOrient="top",
                    labelExpr='datum.value ? "Offline (averaged)" : "Live"',
                    labelOrient="top",
                    labelAnchor="start",
                    labelFontSize=14,
                    labelFontWeight="bold",
                ),
            ),
            tooltip=["count()", "Status:O", "Final probability correct bins:O"],
        )
    )
    return counts_bar.properties(width=fullWidth - 100, height=fullHeight / 4)


# # if we want to see the confidence in detail (more bins)
# def an_overview_of_counts(): # TODO: un-average offline
#     debates['Final probability correct (live + mean of offline)'] = debates.apply(
#         lambda row: row['Final probability correct'] if row['Is offline'] == False else row['Average offline probability correct'], axis=1)
#     bins = [0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1]
#     labels = ['0-10%', '10-20%', '20-30%', '30-40%', '40-50%',
#               '50-60%', '60-70%', '70-80%', '80-90%', '90-100%']
#     debates['Final probability correct bins'] = pd.cut(debates['Final probability correct (live + mean of offline)'],
#                                                        bins=bins, labels=labels)
#     counts_bar = alt.Chart(debates).mark_bar().encode(
#         x=alt.X('count()', stack='zero'),
#         y=alt.Y('Status:O'),
#         color=alt.condition(alt.datum['Final probability correct bins'] == None, alt.value('grey'),
#                             alt.Color('Final probability correct bins:O', sort='descending',
#                                       scale=alt.Scale(scheme='purpleorange'),
#                                       legend=alt.Legend(title='Final probability correct', values=labels))),
#         tooltip=['count()', 'Status:O', 'Final probability correct bins:O']
#     ).facet(facet='Is offline:O', columns=1)
#     return counts_bar.properties(width=fullWidth, height=fullHeight)


# RESULTS
def final_probability_correct_distribution_live_vs_offline_debates():  # TODO: un-average offline
    debates["Final probability correct (live and mean of offline)"] = debates.apply(
        lambda row: row["Final probability correct"]
        if row["Is offline"] == False
        else row["Average offline probability correct"],
        axis=1,
    )
    bars = (
        alt.Chart(debates)
        .mark_bar(opacity=0.75, binSpacing=0.5)
        .encode(
            x=alt.X(
                "Final probability correct (live and mean of offline):Q",
                title="Final probability correct",
                bin=alt.Bin(extent=[0, 1], step=0.05),
            ),
            y=alt.Y("count()", stack=None, title="Number of debates"),
            color=alt.Color(
                "Is offline:O",
                scale=alt.Scale(range=[onlineColor, offlineColor]),
                legend=alt.Legend(
                    title=None,
                    orient="top",
                    labelExpr='datum.value ? "Offline (averaged)" : "Live"',
                    labelFontSize=14,
                    labelFontWeight="bold",
                ),
            ),
            tooltip=["count()", "Is offline:O"],
        )
    )
    return bars.properties(width=fullWidth, height=fullHeight)


# For later when we have accuracy by round, will probably need to change to probability correct each round instead of final
# def accuracy_by_round_live_vs_offline_debates(): # TODO: un-average offline
#     debates['Final probability correct (live + mean of offline)'] = debates.apply(
#         lambda row: row['Final probability correct'] if row['Is offline'] == False else row['Average offline probability correct'], axis=1)
#     boxes = alt.Chart(debates).mark_boxplot().encode(
#         x=alt.X('Number of continues:Q'),
#         y=alt.Y('Final probability correct (live + mean of offline)'),
#     )
#     return boxes.facet(facet='Is offline:O', columns=1).properties(width=fullWidth, height=fullHeight)


def evidence_by_rounds():
    evidence_average = (
        alt.Chart(turns)
        .mark_line(color=aggColor)
        .encode(x="Num previous debating rounds:O", y="mean(Quote length)")
    ).properties(width=fullWidth / 2, height=fullHeight)
    source = turns.merge(
        debates[["Room name", "Honest debater", "Dishonest debater"]],
        how="left",
        on="Room name",
    )
    source["As honest debater"] = source["Participant"] == source["Honest debater"]
    evidence_honest_dishonest = (
        alt.Chart(source)
        .mark_line()
        .encode(
            x="Num previous debating rounds:O",
            y="mean(Quote length)",
            color=alt.Color(
                "As honest debater:N",
                scale=alt.Scale(
                    domain=[True, False], range=[correctColor, incorrectColor]
                ),
            ),
        )
    ).properties(width=fullWidth / 2, height=fullHeight)
    return (evidence_average | evidence_honest_dishonest).configure_axis(labelAngle=0)


def evidence_by_rounds_and_participant():
    evidence_line = (
        alt.Chart(turns)
        .mark_line()
        .encode(
            x="Num previous debating rounds:O",
            y="mean(Quote length)",
            color="Participant:N",
        )
    )
    nearest = alt.selection(
        type="single",
        nearest=True,
        on="mouseover",
        fields=["Num previous debating rounds"],
        empty="none",
    )
    selectors = (
        alt.Chart(turns)
        .mark_point()
        .encode(
            x="Num previous debating rounds:O",
            opacity=alt.value(0),
        )
        .add_selection(nearest)
    )
    text = evidence_line.mark_text(align="left", dx=3, dy=-3).encode(
        text=alt.condition(nearest, "Participant:N", alt.value(" "))
    )
    return alt.layer(evidence_line, selectors, text, data=turns).properties(
        width=fullWidth, height=fullHeight
    )


# def evidence_by_rounds(): #faceted by participant
#     evidence_line = alt.Chart(turns[turns[['Start time'] >
#                                           pd.to_datetime('10/02/23', format='%d/%m/%y')]]).mark_area().encode(
#         x='Num previous debating rounds:O',
#         y='ci0(Quote length)',
#         y2='ci1(Quote length)'
#     ).facet(
#         facet='Participant:N',
#         columns=6
#     ).properties(width=fullWidth)
#     # evidence_err = alt.Chart(debates).mark_errorbar().encode(
#     #     x='Num previous debating rounds:O',
#     #     y='ymin:Q',
#     #     y2='ymax:Q'
#     # )
#     return evidence_line  # + evidence_err


def final_probability_by_debaters():  # I feel like there should be a shorter way to do this... oh well for now
    honest_avg = (
        alt.Chart(debates)
        .mark_circle(color=correctColor)
        .encode(
            x=alt.X(
                "Honest debater:O",
                sort=alt.EncodingSortField(
                    field="Final probability correct", op="mean", order="descending"
                ),
            ),
            y=alt.Y(
                "mean(Final probability correct):Q",
                scale=alt.Scale(domain=[0, 1]),
                title="Average final probability correct +CI",
            ),
        )
    )
    honest_err = (
        alt.Chart(debates)
        .mark_rule()
        .encode(
            x=alt.X(
                "Honest debater:O",
                sort=alt.EncodingSortField(
                    field="Final probability correct", op="mean", order="descending"
                ),
            ),
            y="ci0(Final probability correct)",
            y2="ci1(Final probability correct)",
        )
    )
    dishonest_avg = (
        alt.Chart(debates)
        .mark_circle(color=incorrectColor)
        .encode(
            x=alt.X(
                "Dishonest debater:O",
                sort=alt.EncodingSortField(
                    field="Final probability incorrect",
                    op="mean",
                    order="descending",
                ),
            ),
            y=alt.Y(
                "mean(Final probability incorrect):Q",
                scale=alt.Scale(domain=[0, 1]),
                title="Average final probability INcorrect +CI",
            ),
        )
    )
    dishonest_err = (
        alt.Chart(debates)
        .mark_rule()
        .encode(
            x=alt.X(
                "Dishonest debater:O",
                sort=alt.EncodingSortField(
                    field="Final probability incorrect",
                    op="mean",
                    order="descending",
                ),
            ),
            y="ci0(Final probability incorrect)",
            y2="ci1(Final probability incorrect)",
        )
    )
    return (honest_avg + honest_err).properties(
        width=fullWidth / 2, height=fullHeight
    ) | (dishonest_avg + dishonest_err).properties(
        width=fullWidth / 2, height=fullHeight
    )


def final_probability_correct_by_judge():
    judge_avg = (
        alt.Chart(debates)
        .mark_circle(color=nullColor)
        .encode(
            x=alt.X(
                "Judge:O",
                sort=alt.EncodingSortField(
                    field="Final probability correct", op="mean", order="descending"
                ),
            ),
            y=alt.Y(
                "mean(Final probability correct):Q",
                scale=alt.Scale(domain=[0, 1]),
                title="Average final probability correct +CI",
            ),
        )
    )
    judge_err = (
        alt.Chart(debates)
        .mark_rule()
        .encode(
            x=alt.X(
                "Judge:O",
                sort=alt.EncodingSortField(
                    field="Final probability correct", op="mean", order="descending"
                ),
            ),
            y="ci0(Final probability correct)",
            y2="ci1(Final probability correct)",
        )
    )
    return (judge_avg + judge_err).properties(width=fullWidth, height=fullHeight)


def final_probability_correct_by_judge_experience():  # TODO: add other judge setups
    debates.sort_values(by=["End time"], inplace=True)
    debates["Judge experience"] = debates.groupby("Judge")["End time"].transform(
        "cumcount"
    )
    judge_ex_agg = (
        alt.Chart(debates)
        .transform_filter(datum["Final probability correct"] != None)
        .transform_aggregate(
            mean="mean(Final probability correct)",
            median="median(Final probability correct)",
            groupby=["Judge experience"],
        )
        .transform_fold(
            ["mean", "median"], as_=["Aggregate", "Final probability correct"]
        )
        .mark_line()
        .encode(
            x=alt.X("Judge experience:Q", title="Number of debates judged"),
            y=alt.Y("Final probability correct:Q"),
            strokeDash=alt.StrokeDash(
                "Aggregate:N",
                scale=alt.Scale(domain=["mean", "median"], range=[[1], [10]]),
            ),
            color=alt.value(aggColor),
        )
    )
    # Add error bars
    err = (
        alt.Chart(debates)
        .transform_filter(datum["Final probability correct"] != None)
        .mark_errorband(extent="ci")
        .encode(x=alt.X("Judge experience:Q"), y=alt.Y("Final probability correct:Q"))
    )
    return (judge_ex_agg + err).properties(
        width=fullWidth,
        height=fullHeight,
        title="Average in yellow & median (error bars = IQR) of final probability correct by judge experience",
    )


def final_probability_correct_by_judge_experience_and_participant():  # TODO: add other judge setups # TODO categorize judge patterns
    debates.sort_values(by=["End time"], inplace=True)
    debates["Judge experience"] = debates.groupby("Judge")["End time"].transform(
        "cumcount"
    )
    judge_ex = (
        alt.Chart(debates)
        .mark_line()
        .encode(
            x=alt.X("Judge experience:Q", title="Number of debates judged"),
            y=alt.Y("Final probability correct:Q"),
            color=alt.Color("Judge:N"),
        )
        .transform_filter(datum["Final probability correct"] != None)
    )
    nearest = alt.selection(
        type="single",
        nearest=True,
        on="mouseover",
        fields=["Judge experience:Q"],
        empty="none",
    )
    selectors = (
        judge_ex.mark_point()
        .encode(
            x="Judge experience:Q",
            opacity=alt.value(0),
        )
        .add_selection(nearest)
    )
    text = judge_ex.mark_text(align="left", dx=3, dy=-3).encode(
        text=alt.condition(nearest, "Judge:N", alt.value(" "))
    )
    return alt.layer(judge_ex, selectors, text, data=debates).properties(
        width=fullWidth, height=fullHeight
    )


def final_probability_correct_by_num_judge_rounds():
    source = sessions.merge(
        debates[
            [
                "Room name",
                "Judge",
                "Number of continues",
                "Final probability correct",
                "Speed annotator accuracy",
            ]
        ],
        how="left",
        on="Room name",
    )
    print(source.describe())
    source = source[source["Role"].str.startswith("Debater")]
    source = source.groupby("Room name").mean().reset_index()
    print(source.groupby(["Room name"]).mean())
    base = (
        alt.Chart(source)
        .mark_circle(size=60, color=aggColor)
        .encode(
            x="Number of continues:Q",
            y="Final probability correct:Q",
            # tooltip=["Room name"],
        )
        .properties(width=fullWidth)
        # .transform_filter(datum["Number of continues"])
    )
    mean = (
        alt.Chart(source)
        .mark_line()
        .transform_aggregate(
            mean_prob="mean(Final probability correct)", groupby=["Number of continues"]
        )
        .encode(
            x=alt.X("Number of continues:Q", axis=alt.Axis(values=[1, 2, 3, 4, 5, 6])),
            y="mean_prob:Q",
            # tooltip=["Room name"],
            color=alt.value(aggColor),
        )
        .properties(width=fullWidth)
        # .transform_filter(datum["Number of continues"])
    )
    err = (
        alt.Chart(source)
        .mark_errorband(extent="ci")
        .encode(
            x="Number of continues:Q",
            y="Final probability correct:Q",
            color=alt.value(aggColor),
        )
        .properties(width=fullWidth)
    )
    return (
        base
        + err
        + mean
        # + base.transform_regression(
        #     "Number of continues", "Final probability correct"
        # ).mark_line()
    )


def final_probability_correct_by_information_progress():
    source = sessions.merge(
        debates[
            ["Room name", "Judge", "Number of continues", "Final probability correct"]
        ],
        how="left",
        on="Room name",
    )
    print(source.describe())
    print(source.groupby(["Room name"]).mean())
    source = source[source["Role"].str.startswith("Judge")]
    source = source.groupby("Room name").mean().reset_index()
    return (
        alt.Chart(source)
        .mark_circle(size=60)
        .encode(
            x="factual informativeness (total):O",
            y="Final probability correct:Q",
            tooltip=["subjective correctness"],
        )
        .properties(width=fullWidth)
    )


def final_probability_correct_by_question_sub():
    source = sessions.merge(
        debates[
            ["Room name", "Judge", "Number of continues", "Final probability correct"]
        ],
        how="left",
        on="Room name",
    )
    print(source.describe())
    print(source.groupby(["Room name"]).mean())
    source = source[source["Role"].str.startswith("Debater")]
    source = source.groupby("Room name").mean().reset_index()
    return (
        alt.Chart(source)
        .mark_circle(size=60)
        .encode(x="subjective correctness:O", y="Final probability correct:Q")
        .properties(width=fullWidth)
    )


# def final_probability_correct_over_time(): # TODO: confident mistakes over time?
#     return (
#         alt.Chart(debates)
#         .mark_bar()
#         .encode(x="yearmonthdate(End time):T", y="mean(Final probability correct):Q")
#         .properties(width=fullWidth)
#     )


# def final_probabilitycorrect__by_date(): # buggy
#     debates['Correct'] = debates['Final probability correct'] > 0.5
#     return alt.Chart(debates).mark_bar().encode(
#         x=alt.X('yearmonthdate(End time):O', stack='zero', title='Date',
#                 axis=alt.Axis(labelAngle=90)),
#         y=alt.Y('count():Q'),
#         color=alt.Color('Correct:O', scale=alt.Scale(range=['#fee037', '#57068c']), legend=alt.Legend(
#             title=None, orient='top', labelExpr='datum.value ? "Correct" : "Incorrect"', labelFontSize=14, labelFontWeight='bold')),
#         tooltip=['count()', 'Correct:O']
#     ).transform_filter(
#         datum['Status'] == 'complete'
#     ).properties(width=fullWidth, height=fullHeight)

# TRACK


def num_rounds_per_debate():
    base = (
        alt.Chart(debates)
        .transform_filter('datum["Is over"] == true')
        .transform_joinaggregate(
            groupby=["Is offline"],
            mean_numrounds="mean(Number of debate rounds):Q",
            total="count():Q",
        )
        .transform_calculate(proportion="1 / datum.total")
    )

    num_rounds = base.mark_bar().encode(
        x=alt.X("Number of debate rounds:O", axis=alt.Axis(title="# debate rounds")),
        y=alt.Y("sum(proportion):Q", axis=alt.Axis(format="%", title="% of debates")),
        column="Is offline:N",
        # color='Judge:N',
        tooltip=["count()", "sum(proportion):Q", "mean_numrounds:Q"],
    )

    return num_rounds


def anonymity():
    source = sessions.merge(
        debates[["Room name", "Debater A", "Debater B", "Judge"]],
        how="left",
        on="Room name",
    )
    source["Guesses"] = source.apply(
        lambda row: sum(
            [
                row[col] == row["identity guesses." + col]
                for col in ["Debater A", "Debater B", "Judge"]
            ]
        ),
        axis=1,
    )
    print(source["Guesses"].value_counts)
    return (
        alt.Chart(source)
        .mark_bar()
        .encode(
            x=alt.X("count(Guesses)"),
            y=alt.Y(
                "Participant:O",
                sort=alt.EncodingSortField(op="count", order="descending"),
            ),
            color=alt.Color("Guesses:O"),
        )
    )


def debater_by_turns():
    filtered = sessions[sessions["Role"] != "Offline Judge"]
    blacklist = filtered[
        ~filtered["Participant"].isin(["Emmanuel Makinde", "Max Layden"])
    ]
    source = blacklist.merge(debates, how="left", on="Room name")
    source["Role"] = source["Role"].map(
        lambda x: "Debater" if x.startswith("Debater") else x
    )
    return (
        alt.Chart(source)
        .mark_bar()
        .encode(
            x=alt.X("count()"),
            y=alt.Y(
                "Participant:O",
                sort=alt.EncodingSortField(op="count", order="descending"),
            ),
            color=alt.Color("Role:O"),
            column=alt.Column("Role:O"),
        )
        .transform_filter(datum["Status"] != "complete" & datum["Is turn"] == True)
        .properties(width=200)
    )


def debater_by_turns_weeks():
    debates["End week"] = debates["End time"].apply(lambda x: x.week)

    def convert_time(x):
        start_date = datetime.datetime.strptime(f"{x.year}-{x.week}-1", "%Y-%W-%w")
        end_date = start_date + pd.Timedelta(days=6)
        return f'{start_date.strftime("%b%d")}|{end_date.strftime("%b%d")}'

    debates["End week label"] = debates["End time"].apply(convert_time)
    source = sessions.merge(debates, how="left", on="Room name")
    source["Role"] = source["Role"].map(
        lambda x: "Debater" if x.startswith("Debater") else x
    )
    return (
        alt.Chart(source)
        .mark_bar()
        .encode(
            x=alt.X(
                "End week label:O",
                sort=alt.EncodingSortField(  # TODO: fix, doesn't seem sorted
                    field="End week", order="ascending"
                ),
            ),
            y=alt.Y("count()"),
            color=alt.Color("Role:O"),
        )
        .transform_filter(
            datum["Status"]
            != "complete" & datum["Is turn"]
            == True & datum["Judge"]
            != None
        )
        .properties(width=200)
        .facet(facet="Judge:N", columns=4, spacing=0)
    )


def debater_pairings_by_role():
    return (
        alt.Chart(debates)
        .mark_rect()
        .encode(
            x="Honest debater:O",
            y=alt.Y("Dishonest debater:O", scale=alt.Scale(reverse=True)),
            color="count():Q",
        )
    )


def debater_pairings_by_person():  # TODO Instead of having them in separate items on the dropdown menu, hv multiple tweakers for the same viewed data?
    return


def participant_by_current_workload():
    # debates.set_index('Room name')
    source = sessions.merge(debates, how="left", on="Room name")
    source["Role"] = source["Role"].map(
        lambda x: "Debater" if x.startswith("Debater") else x
    )
    sourceb = source[~source["Participant"].isin(["Emmanuel Makinde", "Max Layden"])]
    return (
        alt.Chart(sourceb)
        .mark_bar()
        .encode(
            x=alt.X("count()"),
            y=alt.Y(
                "Participant:O",
                sort=alt.EncodingSortField(op="count", order="descending"),
            ),
            color=alt.Color("Role:O"),
            column=alt.Column("Role:O"),
        )
        .transform_filter(datum["Is over_x"] == False)
        .properties(width=200)
    )


def participant_by_past_workloads():
    source = sessions.merge(debates, how="left", on="Room name")
    source["Role"] = source["Role"].map(
        lambda x: "Debater" if x.startswith("Debater") else x
    )
    source["End date"] = source["End time"] - pd.to_timedelta(6, unit="d")
    source_by_week = (
        source.groupby(["Participant", pd.Grouper(key="End date", freq="W-MON")])
        .sum()
        .reset_index()
        .sort_values("End date")
    )

    return (
        alt.Chart(source_by_week)
        .mark_bar()
        .encode(
            x=alt.X("count()"),
            y=alt.Y(
                "Participant:O",
                sort=alt.EncodingSortField(op="count", order="descending"),
            ),
            color=alt.Color("End date:O"),
            column=alt.Column("End date:O"),
        )
        .transform_filter(datum["Status"] == "complete")
        .properties(width=200)
    )


def judge_pairings():
    return (
        alt.Chart(
            debates.melt(
                id_vars="Judge",
                value_vars=("Honest debater", "Dishonest debater"),
                value_name="Debater",
            )
        )
        .mark_rect()
        .encode(
            x="Judge:O",
            y=alt.Y("Debater:O", scale=alt.Scale(reverse=True)),
            color="count():Q",
        )
    )


def debates_completed_per_week():
    # debates['End date'] = debates['End time'] - pd.to_timedelta(6, unit='d')

    debates["End week"] = debates["End time"].apply(
        lambda x: x.week
        # lambda x: f'{(x.dt.week).strftime("%b %d")} - {x.strftime("%B %d")}'
    )

    def convert_time(x):
        start_date = datetime.datetime.strptime(f"{x.year}-{x.week}-1", "%Y-%W-%w")
        end_date = start_date + pd.Timedelta(days=6)
        return f'{start_date.strftime("%b%d")}|{end_date.strftime("%b%d")}'

    debates["End week label"] = debates["End time"].apply(
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
    all_bar = (
        alt.Chart(debates[debates["Is over"] == True])
        .mark_bar()
        .encode(
            x=alt.X(
                "End week label:O",
                sort=alt.EncodingSortField(field="End week", order="ascending"),
            ),
            y="count():Q",
        )
        .properties(width=fullWidth)
        .configure_axis(labelAngle=0)
    )

    # all_line = alt.Chart(debates[debates["Is over"] == True].melt(id_vars=['Room name', 'End week label', 'End week'], value_vars=('Honest debater', 'Dishonest debater'), value_name='Debater')).mark_line().encode(
    #     x=alt.X('End week label:O', sort=alt.EncodingSortField(
    #         field='End week', order='ascending')),
    #     y='count():Q',
    #     color='Debater:N'
    # ).properties(width=fullWidth)


# Keys must be valid URL paths. I'm not URL-encoding them.
# Underscores will be displayed as spaces in the debate webapp analytics pane.
all_graph_specifications = {
    "Main_results:_An_overview_of_counts": an_overview_of_counts,
    "Results:_Distribution_of_final_probability_correct,_live_vs_offline_debates": final_probability_correct_distribution_live_vs_offline_debates,
    "Results:_Evidence_by_rounds": evidence_by_rounds,
    "Results:_Evidence_by_rounds_and_participant": evidence_by_rounds_and_participant,
    "Results:_Final_probability_by_debaters": final_probability_by_debaters,
    "Results:_Final_probability_correct_by_judge": final_probability_correct_by_judge,
    "Results:_Final_probability_correct_by_judge_experience": final_probability_correct_by_judge_experience,
    "Results:_Final_probability_correct_by_judge_experience_and_participant": final_probability_correct_by_judge_experience_and_participant,
    "Results:_Final_probability_correct_by_num_judge_rounds": final_probability_correct_by_num_judge_rounds,
    "Results:_Final_probability_correct_by_z(feedback)_question_subjectivity": final_probability_correct_by_question_sub,
    "Results:_Final_probability_correct_by_z(feedback)_information_progress": final_probability_correct_by_information_progress,
    # "Results:_Live_debates_accuracy_by_date": live_debates_accuracy_by_date,
    "Track:_Anonymity": anonymity,
    "Track:_Debates_completed_per_week": debates_completed_per_week,
    "Track:_Debater_by_turns_weeks": debater_by_turns_weeks,
    "Track:_Participant_by_current_workload": participant_by_current_workload,
    # "Track:_Participant_by_past_workload": participant_by_past_workload,
    "Track:_Turns_to_complete_by_participant": debater_by_turns,
    "Track:_Debater_pairings_by_role": debater_pairings_by_role,
    # "Track:_Debater_pairings_by_person": debater_pairings_by_person,
    "Track:_Judge_pairings": judge_pairings,
    "Track:_Num_rounds_per_debate": num_rounds_per_debate,
}


@app.get("/all_graphs")
def all_graphs():
    result = sorted(list(all_graph_specifications.keys()))
    return Response(json.dumps(result), mimetype="application/json")


@app.get("/graph/<name>")
def graph(name: str):
    chart_fn = all_graph_specifications.get(name)
    if chart_fn is None:
        abort(404)
    else:
        return chart_fn().to_json(0)


@app.post("/refresh")
def refresh():
    read_data()
    return {}
