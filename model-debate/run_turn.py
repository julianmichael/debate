"""
Runs on a JSONL file that is the output of the WriteDebates.scala script (transcript of a debate).

Useful for getting LM-formatted transcripts of existing debates for few-shot learning.
"""

import argparse
import asyncio
import re
from typing import List, Optional

from chat_client import ChatClient
from fastapi import FastAPI
from pydantic import BaseModel
from utils import load_secrets

from debate import Debater

secrets = load_secrets("SECRETS")
ORG_KEY = secrets["NYU_ORG"]
OPEN_API_KEY = secrets["API_KEY"]
MAX_CONTEXT_LENGTH = {'gpt-4': 8192, 'gpt-3.5-turbo-16k': 16384, 'gpt-4-32k': 32768}

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
    turns: List[Turn]


def replace_quotes(string):
    pattern = r"```(.*?)```"
    repl = r"<quote>\1</quote>"
    result = re.sub(pattern, repl, string)
    return result


def parse_args():
    parser = argparse.ArgumentParser(description='Runs one turn of Debate')
    parser.add_argument('--filename',
                        type=str,
                        required=True,
                        help='Name of the jsonl file of the current transcript of the debate.')
    parser.add_argument('--turn_type', type=str, required=True, help='The type of turn in the debate.')
    return parser.parse_args()


async def debate(input: DebaterTurnInput, turn_type: str):

    story = f"\n\nContext:\n\n{input.story}\n\nQuestion: {input.question}\n\n"
    for turn in input.turns:
        turn.text = replace_quotes(turn.text)

    names = ["Debater A", "Debater B"]
    history = []
    for turn in input.turns:
        if turn.role == "Debater":
            if turn_type == "single debater":
                turn.role = "Consultant"
            else:
                turn.role = names[turn.index]
        history.append((turn.role, turn.text))

    # model = "gpt-4"
    model = "gpt-4-32k"
    # model = "gpt-3.5-turbo-16k"
    client = ChatClient(model=model,
                        org_key=ORG_KEY,
                        api_key=OPEN_API_KEY,
                        max_context_length=MAX_CONTEXT_LENGTH[model])
    debater = Debater(story, input.answers, 0.7, 0, client)
    response = await debater.run_single_turn(history, 750, 500, turn_type)

    return response


if __name__ == "__main__":
    args = parse_args()
    input = DebaterTurnInput.parse_file(args.filename)
    response = asyncio.run(debate(input, args.turn_type))
    print(response)
    with open("response.txt", "w") as f:
        f.write(response)
