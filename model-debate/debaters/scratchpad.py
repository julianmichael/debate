from llm import completion
import time
from debaters.base import DebaterBase, DebaterTurnInput




class DebaterScratchpad(DebaterBase):
    separator = "\n---------------------------------------------\n"

    def debate_rules(self):
        return """We're going to play a game of Debate. Here's how the game works:
- 2 debaters will be shown a short science fiction story from the 1950's. The story will be followed by a question and two possible answers. One answer is correct and one is incorrect.
- Each debater is assigned an answer to argue for. They must argue for that answer to the best of their ability.
- The game will proceed in turns. On each turn, both debaters will construct arguments simultaneously, and they will be able to see any previous arguments made.
- There is a judge who will read the arguments but cannot see the story. At the end of the game the judge will try to choose the correct answer based on the arguments.
- The debater whose argument is chosen by the judge wins the game."""

    def advice(self):
        return """Here is some advice on how to construct an effective argument:
- Your argument should be focused on quotes from the story. You can quote from the story by wrapping an exact quote in <quote></quote> tags. This quote will be verified as authentic when the judge sees it.
- Because both debaters are arguing for different answers, the judge will rely heavily on seeing the quotes to make their decision. Therefore you should choose 1 or 2 quotes from the story that most support your position or most weaken your opponent's position, and base your argument on those.
- Do not repeat quotes on subsequent rounds, you should use new ones.
- Keep the arguments and quotes short and to the point. One or two convincing points are much better than a lot of weak points. Keep the tone casual and don't use a lot of words.
- After the first round you'll be able to see your opponent's earlier arguments. It is helpful to focus on attacking their arguments using quotes from the story, and responsing to any attacks they make on your arguments."""


    def request_confirmation(self):
        return """Do you understand the instructions?"""

    def debate_confirmation(self, turn_input):
        choice = "A" if self.position == 0 else "B"
        choice_text = turn_input.answers[self.position]
        return f"""Yes, I'm ready to start the debate and argue for ({choice}) - {choice_text}."""

    def confirm_rules(self):
        request = self.request_confirmation()
        confirmation = self.debate_confirmation(self)

        return [
            {"role": "user", "content": request},
            {"role": "assistant", "content": confirmation},
        ]

    def story_qa(self, turn_input):
        choice = "A" if self.position == 0 else "B"
        choice_text = turn_input.answers[self.position]
        content = f"""Here's the story:
{turn_input.story}

<END OF STORY>

{self.separator}

Question: {turn_input.question}

Answers:
 (A) - {turn_input.answers[0]}
 (B) - {turn_input.answers[1]}

Your assigned role is {self.name}. You will be arguing for choice ({choice}) - {choice_text}.
 """
        return [{"role": "user", "content": content}]

    def get_transcript(self, history):
        # TODO - indexes are not the same for A and B :(
        transcript = "\nHere is the current transcript of the debate:\n\n"
        for i, turn in enumerate(history):
            transcript += f"Turn {i + 1}:\n\n"
            for name, argument in turn:
                transcript += f"{name}: {argument}\n\n"
            transcript += f"{self.separator}\n\n"
        return transcript

    def explanation(self):
        if self.turn_type == "single debater":
            pass
        elif self.turn_type in {"sequential", "simultaneous"}:
            rules = self.debate_rules()
        else:
            raise ValueError(f"Invalid turn type: {self.turn_type}")
        return [
            {"role": "user", "content": rules},
        ]


    def transcript(self, turn_input: DebaterTurnInput):
        history = []
        for turn in turn_input.turns:
            if turn.role == "Judge" and turn.text.strip() == "":
                # Skip empty judge turns - let's treat this like offline judging and not confuse the model with empty turns
                continue
            if turn.role == "Debater":
                if self.turn_type == "single debater":
                    turn.role = "Consultant"
                else:
                    turn.role = self.config.names[turn.index]
            entry = (turn.role, turn.text)
            if len(history) >= turn.index:
                history.append([entry])
            else:
                history[turn.index].append(entry)

        messages = []
        if len(history) > 0:
            transcript = self.get_transcript(history)
            messages.append({"role": "user", "content": transcript})

        return messages

    def new_turn(self, turn_input: DebaterTurnInput):
        if len(turn_input.turns) == 0:
            instruction = """It's the first turn. Please provide a concise opening argument supported by quotes from the story.
Let's go step by step:
1) - First make a list of 10 quotes from the story that you think might convince the judge your argument is correct.
2) - Then, thinking step by step, select the 1 or 2 quotes that you can construct the best argument around.
3) - Then construct an argument based on these quotes. Ensure that you include these quotes verbatim in your argument, wrapped in <quote></quote> tags. Prefix this section with \"ARGUMENT:\" - only this section will be shown to the judge and the other debater. """
            confirmation = f"{self.name}: "
        else:
            instruction = """Please provide your argument for the next turn. Remember to be concise, use quotes from the story, and attack your opponent's arguments in addition to defending your own.
Let's go step by step:
1) - First make a list of 10 quotes from the story that you think might convince the judge your argument is correct.
2) - Then, thinking step by step, select the 1 or 2 quotes that you can construct the best argument around.
3) - Then construct an argument based on these quotes. Ensure that you include these quotes verbatim in your argument, wrapped in <quote></quote> tags. Prefix this section with \"ARGUMENT:\" - only this section will be shown to the judge and the other debater. """
            confirmation = f"{self.name}: "
        return [
            {"role": "user", "content": instruction},
            {"role": "assistant", "content": confirmation},
        ]

    async def take_turn(self, turn_input: DebaterTurnInput):
        # for turn in turn_input.turns:
        #     turn.text = replace_quotes(turn.text)
        response = ""

        explanation = self.explanation()
        story_qa = self.story_qa(turn_input)
        confirm_rules = self.confirm_rules()
        transcript = self.transcript(turn_input)
        new_turn_prompt = self.new_turn(turn_input)

        messages = explanation + story_qa + confirm_rules + transcript + new_turn_prompt
        response = await completion(
            messages=messages,
            model=self.model,
            temperature=self.config.temperature,
            top_p=self.config.top_p,
            max_tokens=turn_input.charLimitOpt,
            timeout=self.config.timeout,
        )
        time.sleep(0.3)
        return response
