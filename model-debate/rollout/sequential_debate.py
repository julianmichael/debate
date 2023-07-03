# Adapted from: https://github.com/akbir/debate

import aiohttp

import prompts
from rollout.debate_client import DebateClient
from typing import List

class SequentialDebater():

    def __init__(self, story: str, answers: List[str], temperature: float, debater_idx: int, client: DebateClient):
        super().__init__()
        self.story = story
        self.answers = answers
        self.debater_idx = debater_idx
        self.name = prompts.NAME_1 if debater_idx == 0 else prompts.NAME_2
        self.results = []
        self.client = client

        self.private1 = prompts.private1 + f"You argue that the answer is: '{self.answers[0]}'"
        self.private2 = prompts.private2 + f"You argue that the answer is: '{self.answers[1]}'"

        self.system_prompt = "\n".join([prompts.seq_rules, self.private1] if debater_idx ==
                                       0 else "\n".join([prompts.seq_rules, self.private2]))

        self.temperature = temperature

        self.opening_prompt = f""""
Here the current transcript of the debate is:

{self.story}

{prompts.NAME_1}: I claim the answer is {self.answers[0]}
{prompts.NAME_2}: I claim the answer is {self.answers[1]}
---------------------------------------------\n\n
"""

    async def run_single_turn(self, history):
        separator = "\n---------------------------------------------\n"
        history_str = separator.join([f"{name}: {argument}" for name, argument in history])
        transcript = self.opening_prompt + history_str
        async with aiohttp.ClientSession() as session:

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
