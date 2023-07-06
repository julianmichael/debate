# Adapted from: https://github.com/akbir/debate

import aiohttp

import prompts
from rollout.debate_client import DebateClient
from typing import List

from textwrap import dedent

class SequentialDebater():

    def __init__(self, story: str, answers: List[str], temperature: float, debater_idx: int, client: DebateClient):
        super().__init__()
        self.story = story
        self.answers = answers
        self.debater_idx = debater_idx
        self.name = prompts.NAME_1 if debater_idx == 0 else prompts.NAME_2
        self.results = []
        self.client = client

        # self.private1 = prompts.private1 + f"You argue that the answer is: '{self.answers[0]}'"
        # self.private2 = prompts.private2 + f"You argue that the answer is: '{self.answers[1]}'"
        self.private = prompts.private1 if debater_idx == 0 else prompts.private2
        self.position = self.private + f"You argue that the answer is: '{self.answers[debater_idx]}'"
        self.system_prompt = "\n".join([prompts.seq_rules, self.position])

        self.temperature = temperature


    def prepare_transcript(self, history):
        # TODO: check length of transcript and cut it down to length if possible
        # tip: use something computed from self.client.max_context_size
        separator = "\n---------------------------------------------\n"
        opening_prompt = dedent(f"""
            Here is the current transcript of the debate:

            {self.story}

            {prompts.NAME_1}: I claim the answer is {self.answers[0]}
            {prompts.NAME_2}: I claim the answer is {self.answers[1]}{separator}\n""")
        history_str = separator.join([f"{self.name}: {argument}" for name, argument in history])
        transcript = opening_prompt + history_str + separator + f"{self.name}: "
        return transcript

    async def run_single_turn(self, history):
        transcript = self.prepare_transcript(history)
        async with aiohttp.ClientSession() as session:
            with open("last_system_prompt.txt", "w") as f:
                f.write(self.system_prompt)
            with open("last_transcript_prompt.txt", "w") as f:
                f.write(transcript)

            with open("last_prompt.txt", "w") as f:
                f.write(
                    self.system_prompt + "\n" +
                    "\n\n-------- END SYSTEM PROMPT ------------------\n\n" +
                    transcript
                )

            response = await self.client.chat_completion_with_backoff_async(
                session=session,
                messages=[
                    {
                        "role": "system",
                        "content": self.system_prompt
                    },
                    {
                        "role": "user",
                        "content": transcript,
                    },
                    {
                        "role": "user",
                        "content": f"Complete the next turn of debate as your role of {self.name}:",
                    },
                ],
                temperature=self.temperature,
            )
        return response
