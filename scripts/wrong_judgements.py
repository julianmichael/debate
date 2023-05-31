import os
import pandas as pd

data_dir = os.environ.get("DATA_DIR", default="save")

global debates
global sessions
global turns
debates = pd.read_csv(os.path.join(data_dir, "official/summaries/debates.csv"), keep_default_na=True)
debates["Start time"] = pd.to_datetime(debates["Start time"], unit="ms")
debates["End time"] = pd.to_datetime(debates["End time"], unit="ms")
# only include debates after the given time
debates = debates[
    (debates["Start time"] > pd.to_datetime("10/02/23", format="%d/%m/%y")) &
    (debates["End time"] < pd.to_datetime("26/05/23", format="%d/%m/%y"))
]
## print((debates.sort_values(by=["End time"])).tail(10))
debates["Final probability incorrect"] = 1 - debates["Final probability correct"]
sessions = pd.read_csv(os.path.join(data_dir, "official/summaries/sessions.csv"), keep_default_na=True)
# filter sessions to only the included debates
sessions = sessions.merge(debates[["Room name"]], how="inner", on="Room name")
turns = pd.read_csv(os.path.join(data_dir, "official/summaries/turns.csv"), keep_default_na=True)
turns["Room start time"] = pd.to_datetime(turns["Room start time"], unit="ms")
turns["Speech time"] = pd.to_datetime(turns["Speech time"], unit="ms")
# filter turns to only the included debates
turns = turns.merge(debates[["Room name"]], how="inner", on="Room name")

# print("Debates:")
# print(debates.dtypes)
# print(debates.describe())
# print("Turns:")
# print(turns.dtypes)
# print(turns.describe())
# print((turns[turns["Role"] == "Judge"].sort_values(by=["Speech time"])).tail(10))
# print((turns[turns["Role"] == "Offline Judge"].sort_values(by=["Speech time"])).tail(10))
# print("Sessions:")
# print(sessions.dtypes)
# print(sessions.describe())

sessions = sessions.merge(debates[["Room name", "Is offline", "Honest debater", "Dishonest debater", "Speed annotator accuracy", "Start time", "Question"]], how="left", on="Room name")
print("Sessions merged with debates:")
# print(sessions.dtypes)
print(sessions.shape)

judgements = sessions[sessions['Final probability correct'].notnull()]
print("Only sessions rows of judgments:")
# print(judgments.dtypes)
print(judgements.shape)
judgements = judgements.dropna(axis=1, how='all')
print("Only sessions columns of judgments:")
print(judgements.shape)

wrong_judgements = judgements[judgements["Final probability correct"] < 0.5]
print("Wrong judgements:")
print(wrong_judgements.shape)

# compare the wrong judgements to the correct judgements for debates with the same Question
correct_judgements = judgements[(judgements["Final probability correct"] > 0.5) & (judgements["Question"].isin(wrong_judgements["Question"]))]
print("Correct judgements:")
print(correct_judgements.shape)

compare_judgements = pd.concat([wrong_judgements, correct_judgements], ignore_index=True)
compare_judgements = compare_judgements.drop(columns=["Room start time","Is turn", "Is over", 
                                                      "identity guesses.Judge", "identity guesses.Debater A", "identity guesses.Debater B",
                                                      "Honest debater", "Dishonest debater"])
compare_judgements_sorted = compare_judgements.sort_values(by=["Room name","Start time"])
by_question = compare_judgements_sorted.groupby("Question").apply(
    lambda x: ((x["Final probability correct"] > 0.5).sum()) / (len(x))
)
by_room = compare_judgements_sorted.groupby("Room name").apply(
    lambda x: ((x["Final probability correct"] > 0.5).sum()) / (len(x))
)
compare_judgements_sorted['Question judge accuracy'] = compare_judgements_sorted["Question"].map(by_question)
compare_judgements_sorted['Room judge accuracy'] = compare_judgements_sorted["Room name"].map(by_room)
# print(compare_judgements_sorted)
# print(compare_judgements_sorted['Judge accuracy of question'].describe())

debater_feedback = sessions[sessions['Role'].str.startswith("Debater")]
debater_feedback = debater_feedback.dropna(axis=1, how='all')
debater_feedback = debater_feedback.drop(columns=["Room start time","Is turn", "Is over",
                                                  "identity guesses.Judge", "identity guesses.Debater A", "identity guesses.Debater B",
                                                  'Is offline','Speed annotator accuracy', 'Start time', 'Question'])
# print(debater_feedback.columns)
# print(debater_feedback.shape)
dishonest_debater_feedback = debater_feedback[debater_feedback["Participant"] == debater_feedback["Dishonest debater"]]
honest_debater_feedback = debater_feedback[debater_feedback["Participant"] == debater_feedback["Honest debater"]]
dishonest_debater_feedback = dishonest_debater_feedback.drop(columns=["Dishonest debater", "Honest debater"])
honest_debater_feedback = honest_debater_feedback.drop(columns=["Dishonest debater", "Honest debater"])
# print(dishonest_debater_feedback.shape)
# print(honest_debater_feedback.shape)
both_debaters_feedback = dishonest_debater_feedback.merge(honest_debater_feedback, on="Room name", suffixes=("_dishonest", "_honest"))

final_judgements = compare_judgements_sorted.merge(both_debaters_feedback, how="left", on="Room name")
print("Final judgements:")
print(final_judgements.shape)

# save to csv
final_judgements.to_csv(os.path.join(data_dir, "official/summaries/final_judgements.csv"), index=False)