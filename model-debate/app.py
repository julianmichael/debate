import re
from typing import List, Optional

from fastapi import FastAPI
from pydantic import BaseModel

from rollout.debate_client import DebateClient
from rollout.sequential_debate import SequentialDebater
from utils import load_secrets

secrets = load_secrets("SECRETS")
ORG_KEY = secrets["NYU_ORG"]
OPEN_API_KEY = secrets["API_KEY"]
MAX_CONTEXT_LENGTH = {'gpt-4': 8192, 'gpt-3.5-turbo-16k': 16384}

app = FastAPI()


class Turn(BaseModel):
    role: str
    index: Optional[int]
    text: str
    probabilities: Optional[List[float]]


class DebaterTurnInput(BaseModel):
    storyId: str
    storyTitle: str
    story: str
    question: str
    answers: List[str]
    debaterIndex: int
    turns: List[Turn]
    charLimitOpt: int
    quoteCharLimitOpt: int
    isSimultaneous: bool


def replace_quotes(string):
    pattern = r"```(.*?)```"
    repl = r"<quote>\1</quote>"
    result = re.sub(pattern, repl, string)
    return result


@app.post("/debate")
async def debate(input: DebaterTurnInput):

    story = f"\n\nContext:\n\n{input.story}\n\nQuestion: {input.question}\n\n"
    for turn in input.turns:
        turn.text = replace_quotes(turn.text)

    names = ["Debater A", "Debater B"]
    history = []
    for turn in input.turns:
        if turn.role == "Debater":
            turn.role = names[turn.index]
        history.append((turn.role, turn.text))

    # model = "gpt-4"
    model = "gpt-3.5-turbo-16k"
    client = DebateClient(model = model, org_key= ORG_KEY, api_key = OPEN_API_KEY, max_context_length=MAX_CONTEXT_LENGTH[model])
    debater = SequentialDebater(story, input.answers, 0.7, input.debaterIndex, client)
    response = await debater.run_single_turn(history)

    return response
