import argparse
import asyncio
import json
import os
from datetime import datetime
from typing import List, Tuple

import pandas as pd
import tiktoken

from rollout.sequential_debate import SequentialDebater
from utils import load_secrets

secrets = load_secrets("SECRETS")
ORG_KEY = secrets["NYU_ORG"]
OPEN_API_KEY = secrets["API_KEY"]
ANTHROPIC_API_KEY = secrets["ANTHROPIC_API_KEY"]
ARTICLE_LEN_LIMIT = {'gpt-4': 6000, 'gpt-3.5-turbo': 2000}


def filter_on_story_length(story, model):
    encoding = tiktoken.encoding_for_model(model)
    article_len = len(encoding.encode(story))
    if article_len >= ARTICLE_LEN_LIMIT['gpt-4']:
        raise ValueError(
            f"Article length {article_len} is too long for model {model} with limit {ARTICLE_LEN_LIMIT[model]}")


def load_story_and_question(story_idx, question_idx, model):

    def read_jsonl(path):
        # Manually open because .splitlines is different from iterating over lines
        ls = []
        with open(path, "r") as f:
            for line in f:
                ls.append(json.loads(line))
        return ls

    val_dataset = read_jsonl('data/quality/QuALITY.v1.0.1/QuALITY.v1.0.1.htmlstripped.dev')
    story = val_dataset[story_idx]['article']
    filter_on_story_length(story, model)

    question = val_dataset[story_idx]['questions'][question_idx]['question']
    answer_choices = val_dataset[story_idx]['questions'][question_idx]['options']
    gold_label = val_dataset[story_idx]['questions'][question_idx]['gold_label']
    print(f"Difficult: {val_dataset[story_idx]['questions'][question_idx]['difficult']}")

    correct_answer = answer_choices[gold_label]
    incorrect_answer = answer_choices[0 if gold_label != 0 else 1]

    return {"story": story, "question": question, "correct answer": correct_answer, "negative answer": incorrect_answer}


async def main():

    parser = argparse.ArgumentParser()
    parser.add_argument("--transcript", help="filename of transcript")
    parser.add_argument("--output", help="filename of output")
    parser.add_argument("--model", help="model to use")
    parser.add_argument("--num_steps", default=2, type=int)
    parser.add_argument("--temperature", default=0.7, type=float)
    parser.add_argument("--test", action=argparse.BooleanOptionalAction)
    parser.add_argument("--model_role", help="role of model: debater_a, debater_b")

    args = parser.parse_args()
    # add the current datetime to the filename
    now = datetime.now().strftime("%Y-%m-%d-%H-%M")
    output_filename = os.path.join('data', 'outputs', f"{now}_{args.output}.txt")
    MODEL = args.model
    NUM_STEPS = args.num_steps
    TEMPERATURE = args.temperature
    MODEL_ROLE = args.model_role

    # load story, question, answer choices
    data = load_story_and_question(1, 0, MODEL)
    data['story'] = f"\n\nContext:\n\n{data['story']}\n\nQuestion: {data['question']}\n\n"
    data['debater_a_answer'] = data['correct answer']
    data['debater_b_answer'] = data['negative answer']

    print(f"Question: {data['question']}")
    print(f"Correct answer: {data['correct answer']}")
    print(f"Negative answer: {data['negative answer']}")

    # load history
    history: List[Tuple[str, str]] = []
    data['history'] = history

    api_key = ANTHROPIC_API_KEY if MODEL.startswith("claude") else OPEN_API_KEY
    debater = SequentialDebater(data, MODEL, NUM_STEPS, TEMPERATURE, MODEL_ROLE, api_key, ORG_KEY)

    if args.model_role == "debater_a":
        response = await debater.run_single_turn()
        print(response)
    while True:
        try:
            human_response = input("Your response: ")
            debater.history.append(("Human", human_response))
            model_response = await debater.run_single_turn()
            print(model_response)
        except KeyboardInterrupt:
            break
        except Exception as e:
            print(e)
            with open(output_filename, 'w') as f:
                f.write(debater.create_transcript())
            return

    with open(output_filename, 'w') as f:
        f.write(debater.create_transcript())


if __name__ == "__main__":
    asyncio.run(main())
