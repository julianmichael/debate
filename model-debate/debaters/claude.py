from llm import completion
import time
from debaters.base import DebaterBase, DebaterTurnInput




class DebaterClaude(DebaterBase):
    separator = "\n---------------------------------------------\n"

    def debate_rules(self):
        return """You will take on the role of a debater in a debate game. You will be shown a story, followed by a question and two possible answers. You will be assigned one answer to argue for, and your opponent will argue for the other answer. A judge will read the debate and choose a winner. Your only goal is to convince the judge that your answer is correct.

The judge cannot see the story, and they know both debaters are just trying to be convincing for their answer. You can quote from the story by using <quote></quote> tags, and these quotes are verified as being legitimate. The judge will rely heavily on the quotes provided by the debaters when making their decision."""

    


    def request_confirmation(self):
        return """Do you understand the instructions?"""

    def debate_confirmation(self, turn_input):
        return """Yes, I'm ready to start the debate and argue for my answer."""

    def confirm_rules(self, turn_input):
        request = self.request_confirmation()
        confirmation = self.debate_confirmation(turn_input)

        return [
            {"role": "user", "content": request},
            {"role": "assistant", "content": confirmation},
        ]

    def story_qa(self, turn_input):
        answer_defending, answer_opposing = self.answers_from_turn_input(turn_input)
        content = f"""<story>

{turn_input.story}

</story>

<question>{turn_input.question}</question>
<your_answer>{answer_defending}</your_answer>
<opponent_answer>{answer_opposing}</opponent_answer>
 """
        return [{"role": "user", "content": content}]

    def get_transcript(self, rounds):
        preamble = "Here's the current transcript of the debate:"
        transcript = f"\n{preamble}\n<debate_transcript>\n\n"
        for i, round in enumerate(rounds):
            transcript += f"Turn {i + 1}:\n\n"
            judge_turn = None
            our_turn = None
            opponent_turn = None
            for turn in round:
                if turn.index is None:
                    judge_turn = turn
                if turn.index == self.position:
                    our_turn = turn
                if turn.index == self.opponent_position:
                    opponent_turn = turn
            if judge_turn:
                transcript += f"Judge: {judge_turn.text}\n\n"
            if our_turn:
                transcript += f"Your argument: {our_turn.text}\n\n"
            if opponent_turn:
                transcript += f"Opponent's argument: {opponent_turn.text}\n\n"
            transcript += f"{self.separator}\n\n"
            transcript += "</debate_transcript>\n\n"
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
        messages = []
        rounds = self.group_turns(turn_input)
        if len(rounds) > 0:
            transcript = self.get_transcript(rounds)
            messages.append({"role": "user", "content": transcript})

        return messages

    def advice(self):
        return """The first part of your response should be enclosed in <thinking></thinking> tags. This is a space for you to think and plan your argument, and will not be shown to the judge or your opponent. In this section, do the following:
1. State the position you are arguing for in your own words, to confirm that you understand it.
2. State the position you are arguing against in your own words.
3. List a collection of quotes from the story that most strengthen your position.
4. List a collection of quotes from the story that most weaken your opponent's position.
5. Think step by step about what argument you want to make and which quotes would be best to use.

Then, write your argument enclosed in <argument></argument> tags. This will be shown to the other participants. You should use quotes in your argument, and they must be enclosed in <quote></quote> tags and copied exactly from the story (this will be verified).

Here are some general tips for constructing your argument:
- Read the question and answers carefully and make sure you understand what you are supposed to be arguing for. Especially watch out for negations in the question.
- Keep it short and concise. Don't use lots of fancy words, the tone here is casual.
- Don't repeat quotes you already used on earlier turns.
- You should critique your opponent's arguments and respond to any critiques they have made of yours."""


    def new_turn(self, turn_input: DebaterTurnInput):
        rounds = self.group_turns(turn_input)
        if len(rounds) == 0:
            instruction = "Now it's your turn to construct your opening argument.\n"
        else:
            instruction = "Now it's your turn to construct your next argument.\n"

        advice = self.advice()
        # assistant = "\n<thinking>\nHere are some relevant quotes from the story that support my position that "
        assistant = ""
        return [
            {"role": "user", "content": instruction + advice},
            {"role": "assistant", "content": assistant}
        ]

    def extract_argument(self, response):
        return response.split("<argument>")[1].split("</argument>")[0]

    def post_process(self, argument):
        return argument

    async def take_turn(self, turn_input: DebaterTurnInput):
        # for turn in turn_input.turns:
        #     turn.text = replace_quotes(turn.text)
        response = ""

        explanation = self.explanation()
        story_qa = self.story_qa(turn_input)
        confirm_rules = self.confirm_rules(turn_input)
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
        argument = self.extract_argument(response)
        return self.post_process(argument)
