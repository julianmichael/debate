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

    def qa(self, turn_input):
        answer_defending, answer_opposing = self.answers_from_turn_input(turn_input)
        qa = f"""Here is the question and the two answers:
<question>{turn_input.question}</question>
<your_answer>{answer_defending}</your_answer>
<opponent_answer>{answer_opposing}</opponent_answer>

Please restate your position and your opponent's position to confirm you understand."""
        confirmation = f"""I understand. I will argue that the answer to the question \"{turn_input.question}\" is \"{answer_defending}\". I will argue against my opponent's answer \"{answer_opposing}\"."""

        return [
            {"role": "user", "content": qa},
            {"role": "assistant", "content": confirmation},
        ]


    def story(self, turn_input):
        content = f"""<story>
 {turn_input.story}
</story>
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

    def advice(self, turn_input):
            
        first_round_thinking = """1. State the question.
2. State the position you are arguing for in your own words.
3. State the position you are arguing against in your own words.
4. Make a list of 10 quotes from the story that most strengthen your position.
5. Think step by step about which quotes you can construct the strongest argument around.
"""
        second_round_thinking = """1. State the question.
2. State the position you are arguing for in your own words.
3. State the position you are arguing against in your own words.
4. List out biggest flaws in your opponent's argument. Are the quotes they have shown misleading in some way? Are their arguments not well supported by the evidence? Are their arguments irrelevant to the question? Are their arguments weak in some other way?
5. Make a list of 5 quotes from the story that contradict your opponent's argument or weaken it.
6. Think step by step about which quotes you can construct the strongest argument around. You should focus on critiquing the arguments your opponent has made. """
        nth_round_thinking = """1. State the question.
2. State the position you are arguing for in your own words.
3. State the position you are arguing against in your own words.
4. List the core critiques the opponent has made to your argument.
5. Make a list of 5 quotes from the story that counter your opponent's critiques or otherwise strengthen your position.
6. List out biggest flaws in your opponent's argument. Are the quotes they have shown misleading in some way? Are their arguments not well supported by the evidence? Are their arguments irrelevant to the question? Are their arguments weak in some other way?
7. Make a list of 5 quotes from the story that contradict your opponent's argument or weaken it.
8. Think step by step about which quotes you can construct the strongest argument around. You should focus on critiquing the arguments your opponent has made and countering the critiques they have made.
"""

        rounds = self.group_turns(turn_input)
        if len(rounds) == 0:
            thinking = first_round_thinking
        elif len(rounds) == 1:
            thinking = second_round_thinking
        else:
            thinking = nth_round_thinking
        advice =  f"""The first part of your response should be enclosed in <thinking></thinking> tags. This is a space for you to think and plan your argument, and will not be shown to the judge or your opponent. In this section, do the following:
{thinking}

Then, write your argument enclosed in <argument></argument> tags. This will be shown to the other participants. You should use some of the quotes you listed earlier in your argument, and they must be enclosed in <quote></quote> tags and copied exactly from the story (this will be verified).

Here are some general tips for constructing your argument:
- Read the question and answers carefully and make sure you understand what you are supposed to be arguing for. Especially watch out for negations in the question.
- Keep your argument short and concise. Just a few sentences.
- Don't use lots of fancy words, the tone here is casual.
- Use lots of quotes!"""
        
        return advice


    def new_turn(self, turn_input: DebaterTurnInput):
        answer_defending, _ = self.answers_from_turn_input(turn_input)
        if self.is_first_round(turn_input):
            instruction = f"Now it's your turn to construct your opening argument for why the answer to the question \"{turn_input.question} is \"{answer_defending}\".\n"
        else:
            instruction = f"Now it's your turn to construct your next argument for why the answer to the question \"{turn_input.question} is \"{answer_defending}\".\n"

        advice = self.advice(turn_input)
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
        qa = self.qa(turn_input)
        story = self.story(turn_input)
        # confirm_rules = self.confirm_rules(turn_input)
        transcript = self.transcript(turn_input)
        new_turn_prompt = self.new_turn(turn_input)

        messages = explanation + qa + story + transcript + new_turn_prompt
        response = await completion(
            messages=messages,
            model=self.model,
            temperature=self.config.temperature,
            top_p=self.config.top_p,
            max_tokens=turn_input.charLimitOpt,
            timeout=self.config.timeout,
        )
        argument = self.extract_argument(response)
        return self.post_process(argument)
