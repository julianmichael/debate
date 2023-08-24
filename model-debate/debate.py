# Adapted from: https://github.com/akbir/debate
import argparse
import random
from collections import Counter
import asyncio
import json
from typing import List
from debaters.base import DebaterTurnInput, Turn
from debaters.select_debater import select_debater

DATASET_PATH = "../data/QuALITY.v1.0.1/QuALITY.v1.0.1.htmlstripped.train"

def load_story_and_question(story_id, question_idx):
    def read_jsonl(path):
        # Manually open because .splitlines is different from iterating over lines
        ls = []
        with open(path, "r") as f:
            for line in f:
                ls.append(json.loads(line))
        return ls

    dataset = read_jsonl(DATASET_PATH)
    matches = [s for s in dataset if int(s["article_id"]) == int(story_id)]
    item = matches[-1]
    story = item["article"]
    question_data = item["questions"][question_idx]
    question = question_data["question"]
    answer_choices = question_data["options"]
    gold_label = question_data["gold_label"]
    distractors = [
        item["untimed_best_distractor"]
        for item in question_data["validation"]
        if "untimed_best_distractor" in item
    ]
    # labels are 1-indexed!
    correct_answer = answer_choices[gold_label - 1]

    if distractors:
        most_common_distractor = Counter(distractors).most_common(1)[0][0]
        incorrect_answer = answer_choices[most_common_distractor - 1]
    else:
        incorrect_answer = answer_choices[0 if gold_label - 1 != 0 else 1]

    print(f"Difficult: {question_data['difficult']}")
    return {
        "story": story,
        "question": question,
        "correct answer": correct_answer,
        "negative answer": incorrect_answer,
    }


async def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--debater", help="Name of debater to use")
    parser.add_argument("--model", help="Name of model to use", default="gpt-4")
    parser.add_argument("--story", help="story to use", default=1, type=int)
    parser.add_argument("--question", help="question to use", default=0, type=int)
    parser.add_argument(
        "--turn_type", help="turn role", default="simultaneous", type=str
    )
    parser.add_argument("--position", help="position in debate", default=0, type=int)
    args = parser.parse_args()
    debater = select_debater(args.debater, args.model, args.position, args.turn_type)

    data = load_story_and_question(args.story, args.question)
    answers = [data["correct answer"], data["negative answer"]]
    random.shuffle(answers)  # in-place mod
    print(f"Question: {data['question']}")
    print(f"A: {answers[0]}")
    print(f"B: {answers[1]}")

    turns: List[Turn] = []
    index = 0
    while True:
        try:
            if len(turns) > 0:
                human_input = input("Judge: ")
                turns.append(Turn(role="Judge", text=human_input, index=index))
            turn_input = DebaterTurnInput(
                story=data["story"],
                question=data["question"],
                answers=answers,
                turns=turns,
                charLimitOpt=4000,
                quoteCharLimitOpt=1000,
            )
            response = await debater.take_turn(turn_input)
            turns.append(Turn(role=debater.name, text=response, index=index))
            print(response)
            index += 1
        except KeyboardInterrupt:
            break


if __name__ == "__main__":
    asyncio.run(main())
