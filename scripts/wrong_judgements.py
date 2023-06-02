import os
import pandas as pd  # type: ignore

data_dir = os.environ.get("DATA_DIR", default="save")

global debates
global sessions
global turns
debates = pd.read_csv(
    os.path.join(data_dir, "official/summaries/debates.csv"), keep_default_na=True
)
debates["Start time"] = pd.to_datetime(debates["Start time"], unit="ms")
# only include debates after the given time
debates = debates[debates["Start time"] > pd.to_datetime("10/02/23", format="%d/%m/%y")]
debates["Final probability incorrect"] = 1 - debates["Final probability correct"]
debates["End time"] = pd.to_datetime(debates["End time"], unit="ms")
sessions = pd.read_csv(
    os.path.join(data_dir, "official/summaries/sessions.csv"), keep_default_na=True
)
# filter sessions to only the included debates
sessions = sessions.merge(debates[["Room name"]], how="inner", on="Room name")

# turns = pd.read_csv(
#     os.path.join(data_dir, "official/summaries/turns.csv"), keep_default_na=True
# )
# turns["Room start time"] = pd.to_datetime(turns["Room start time"], unit="ms")
# # filter turns to only the included debates
# turns = turns.merge(debates[["Room name"]], how="inner", on="Room name")


judgements = sessions[sessions["Final probability correct"].notnull()]
judgements = judgements.dropna(axis=1, how="all")
judgements = judgements.drop(columns=["Is turn", "Is over"])


wrong_judgements = judgements[judgements["Final probability correct"] < 0.5]

wrong_judgements_info = wrong_judgements.merge(
    debates[
        [
            "Room name",
            "Is offline",
            "Honest debater",
            "Dishonest debater",
            "Speed annotator accuracy",
            "End time",
        ]
    ],
    how="left",
    on="Room name",
)

wrong_judgements_info = wrong_judgements_info.sort_values(by=["Room name", "End time"])

wrong_judgements_info.to_csv(
    os.path.join(data_dir, "official/summaries/wrong_judgements.csv"), index=False
)
