# Adapted from: https://github.com/akbir/debate

import aiohttp

import prompts
from rollout.debate_manager import DebateManager


class SequentialDebater(DebateManager):

    def __init__(self, data, model, temperature, debater_idx, api_key, org_key):
        super().__init__()
        self.data = data
        self.history = data['history']
        self.model = model
        self.debater_idx = debater_idx
        self.name = prompts.NAME_1 if debater_idx == 0 else prompts.NAME_2
        self.results = []

        self.private1 = prompts.private1 + f"You argue that the answer is: '{self.data['answers'][0]}'"
        self.private2 = prompts.private2 + f"You argue that the answer is: '{self.data['answers'][1]}'"

        self.system_prompt = "\n".join([prompts.seq_rules, self.private1] if debater_idx ==
                                       0 else "\n".join([prompts.seq_rules, self.private2]))

        self.api_key = api_key
        self.org_key = org_key
        self.temperature = temperature

        self.opening_prompt = f""""
Here the current transcript of the debate is:

{data['story']}

{prompts.NAME_1}: I claim the answer is {data['answers'][0]}
{prompts.NAME_2}: I claim the answer is {data['answers'][1]}
---------------------------------------------\n\n
"""

    def join_history(self):
        separator = "\n---------------------------------------------\n"
        return separator.join([f"{name}: {argument}" for name, argument in self.history])

    def create_transcript(self):
        return self.opening_prompt + self.join_history()

    async def run_single_turn(self):
        transcript = self.create_transcript()
        async with aiohttp.ClientSession() as session:

            response = await self.chat_completion_with_backoff_async(
                session=session,
                model=self.model,
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

        self.history.append((self.name, response))
        return response
