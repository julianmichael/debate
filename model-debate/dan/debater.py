import os
import re
import time
from tenacity import RetryError
from fastapi import HTTPException
from importlib.machinery import SourceFileLoader
from typing import Dict, List, Optional
from pydantic import BaseModel
from dan.calls import completion
from dan.utils import load_yaml, replace_quotes


class Turn(BaseModel):
    index: Optional[int]
    role: str
    text: str


class DebaterTurnInput(BaseModel):
    story: str
    question: str
    answers: List[str]
    turns: List[Turn]
    charLimitOpt: int
    quoteCharLimitOpt: int


class DebaterConfig(BaseModel):
    names: List[str]
    consultant_name: str
    temperature: float = 0.2
    top_p: float = 1.0
    timeout: int = 120


class Debater:
    def __init__(self, config_name: str, model: str, position: int, turn_type: str):
        current_directory = os.path.dirname(os.path.abspath(__file__))
        config_dir = os.path.join(current_directory, "configs", config_name)
        self.config = DebaterConfig(
            **load_yaml(os.path.join(config_dir, "config.yaml"))
        )
        self.model = model
        prompts_module = SourceFileLoader(
            "prompts", os.path.join(config_dir, "prompts.py")
        ).load_module()
        self.prompts = prompts_module.Prompts
        self.position = position
        self.opponent_position = 1 - position
        self.turn_type = turn_type
        self.name = (
            self.config.names[position]
            if turn_type != "single debater"
            else self.config.consultant_name
        )
        self.opponent_name = (
            self.config.names[self.opponent_position]
            if turn_type != "single debater"
            else None
        )

    def word_limit_from_char_limit(self, char_limit: int, quote_char_limit: int):
        word_limit = char_limit / 8
        quote_limit = quote_char_limit / 8
        return word_limit, quote_limit

    async def run_single_turn(self, turn_input: DebaterTurnInput):
        # for turn in turn_input.turns:
        #     turn.text = replace_quotes(turn.text)


        response = ""

        messages = self.prompts.construct_messages(self, turn_input)
        try:
            response = await completion(
                messages=messages,
                model=self.model,
                temperature=self.config.temperature,
                top_p=self.config.top_p,
                max_tokens=turn_input.charLimitOpt,
                timeout=self.config.timeout,
            )
        except RetryError:
            raise HTTPException(
                status_code=429, detail="Rate limit exceeded from OpenAI API"
            )
        time.sleep(0.3)
        return response

    def check_output_length(self, output: str, char_limit: int, quote_char_limit: int):
        num_output_chars = len(output)
        pattern = r"<quote>(.*?)</quote>"
        matches = re.findall(pattern, output, re.DOTALL)
        num_quote_chars = sum([len(match) for match in matches])
        if num_output_chars > char_limit:
            return "total", num_output_chars, num_quote_chars
            # don't bother with quote limit. more quotes good
            # elif num_quote_chars > quote_char_limit:
            #     return "quote", num_output_chars, num_quote_chars
        return "ok", num_output_chars, num_quote_chars

    def answers_from_turn_input(self, turn_input: DebaterTurnInput):
        answer_defending = turn_input.answers[self.position]
        answer_opposing = turn_input.answers[self.opponent_position]
        return answer_defending, answer_opposing

    def format_transcript(self, turn_input: DebaterTurnInput):
        if len(turn_input.turns) == 0:
            return FIRST_ROUND_TRANSCRIPT
        else:
            arguments = ""
            for i, round in enumerate(transcript.rounds):
                arguments = (
                    arguments
                    + f"Turn {i + 1}:\n"
                    + "Your argument:\n"
                    + f"{round[self.position - 1]}\n\n"
                    + f"{ARG_SEP}\n\n"
                    + "Your opponent's argument:\n"
                    + f"{round[self.opponent_position - 1]}\n\n"
                    + f"{ROUND_SEP}\n"
                )

            formatted = f"""{TRANSCRIPT_PREAMBLE}

{arguments}
Turn {len(transcript.rounds) + 1}:
Your argument:
"""

            return formatted

    def fill_in_content(self, content: str, turn_input: DebaterTurnInput):
        answer_defending, answer_opposing = self.answers_from_transcript(transcript)
        # formatted_transcript = self.format_transcript(transcript)
        # content = content.replace("<QUESTION>", transcript.question)
        content = content.replace("<ANSWER>", answer_defending)
        content = content.replace("<OPPONENT_ANSWER>", answer_opposing)
        # content = content.replace("<TRANSCRIPT>", formatted_transcript)
        content = content.replace("  ", "")
        return content

    def parse_messages(self, messages: list[dict], turn_input: DebaterTurnInput):
        messages = []
        for message in messages:
            role = message["role"]
            content = message["content"]
            content = self.fill_in_content(content, turn_input)
            messages.append({"role": role, "content": content})
        return messages

    def construct_messages(self, turn_input: DebaterTurnInput):
        # Fill in the content of the prompts
        messages = []
        messages.append(
            self.parse_messages(self.config.messages["explanation"], turn_input)
        )
        return messages

    async def take_turn(self, turn_input: DebaterTurnInput):
        response = await self.get_completion(transcript)
        return response
