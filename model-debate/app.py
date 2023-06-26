import re
from typing import List, Optional

from fastapi import FastAPI
from pydantic import BaseModel

from rollout.sequential_debate import SequentialDebater
from utils import load_secrets

secrets = load_secrets("SECRETS")
ORG_KEY = secrets["NYU_ORG"]
OPEN_API_KEY = secrets["API_KEY"]

app = FastAPI()


class Turn(BaseModel):
    role: str
    index: Optional[int]
    text: str
    probabilities: Optional[List[float]]


class DebateInput(BaseModel):
    storyId: str
    storyTitle: str
    story: str
    question: str
    answers: List[str]
    debateId: str
    judge: str
    turns: List[Turn]


def replace_quotes(string):
    pattern = r"```(.*?)```"
    repl = r"<quote>\1</quote>"
    result = re.sub(pattern, repl, string)
    return result


@app.post("/debate")
async def debate(input: DebateInput):

    story = f"\n\nContext:\n\n{input.story}\n\nQuestion: {input.question}\n\n"
    for turn in input.turns:
        turn.text = replace_quotes(turn.text)

    for i in range(len(input.turns) - 1, -1, -1):
        if input.turns[i].role == "Debater A":
            debater_idx = 1
            break
        elif input.turns[i].role == "Debater B":
            debater_idx = 0
            break

    names = ["Debater A", "Debater B"]
    history = []
    for turn in input.turns:
        if turn.role == "Debater":
            turn.role = names[turn.index]
        history.append((turn.role, turn.text))

    data = {'story': story, 'answers': input.answers, 'history': history}

    debater = SequentialDebater(data, "gpt-4", 0.7, debater_idx, OPEN_API_KEY, ORG_KEY)
    response = await debater.run_single_turn()

    return {"response": response}
