from typing import List, Optional
import os

from fastapi import FastAPI
from pydantic import BaseModel
from utils import load_secrets
from debaters.select_debater import select_debater

from debaters.base import DebaterTurnInput

secrets = load_secrets("SECRETS")
ORG_KEY = secrets["NYU_ORG"]
OPEN_API_KEY = secrets["API_KEY"]
ANTHROPIC_API_KEY = secrets["ANTHROPIC_API_KEY"]
MAX_CONTEXT_LENGTH = {
    "gpt-4": 8192,
    "gpt-3.5-turbo-16k": 16384,
    "gpt-4-32k": 32768,
    "claude-2": 100000,
}

app = FastAPI()


class DebateRequestTurn(BaseModel):
    role: str
    index: Optional[int]
    text: str
    probabilities: Optional[List[float]]


class DebateRequestInput(BaseModel):
    storyId: str
    storyTitle: str
    story: str
    question: str
    answers: List[str]
    debaterIndex: int
    turns: List[DebateRequestTurn]
    charLimitOpt: int
    quoteCharLimitOpt: int
    turnType: str


@app.post("/debate")
async def debate(input: DebateRequestInput):
    # set the model & config on the command line when starting the server - defaults to gpt-4 original
    model = os.getenv("MODEL", "gpt-4")
    debater_name = os.getenv("DEBATER", "original")
    # Change 'None' indexes to 0 (the initial judge index is None)
    input.turns = [DebateRequestTurn(role=turn.role, text=turn.text, index=turn.index or 0, probabilities=None) for turn in input.turns]
    debater = select_debater(debater_name, model, input.debaterIndex, input.turnType)
    turn_input = DebaterTurnInput(**input.model_dump())
    response = await debater.take_turn(turn_input)

    return response
