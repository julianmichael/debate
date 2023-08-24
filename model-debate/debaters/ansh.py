from llm import completion
import time
from debaters.base import DebaterBase, DebaterTurnInput




class DebaterAnsh(DebaterBase):
    separator = "\n---------------------------------------------\n"

    def debate_rules(self):
        return """Study the following long passage of text carefully. I'll present you with a question about the passage after reading it as well as answer choices for that question. I'll ask you to provide a convincing, well-reasoned, and honest argument for a specific answer choice. I'd like your argument to address the strongest possible counterarguments you foresee as well. Importantly, the person to whom you will be presenting this argument won't be able to see the passage - only the question, your argument, and an argument defending the other choice. The person evaluating the arguments will likely need evidence in order to be convinced of your argument, so you should attempt to provide that in the form of quotes from the passage. If there are quotes that appear to support the other answer choice, it may be helpful to explain why they might not actually support that answer, using additional quotes if possible to make your case. Whenever you use a quote from the passage, please include it in <quote></quote> tags, e.g. <quote>He shot the gun.</quote>. Your quotes will be verified to make sure that they actually belong to the passage. Make sure to not omit any text in your quotes, such as with ellipses. Do you understand the instructions?"""

    def single_debater_rules(self):
        return """Study the following long passage of text carefully. I'll present you with a question about the passage after reading it as well as answer choices for that question. I'll ask you to provide a convincing, well-reasoned, and honest argument for a specific answer choice. I'd like your argument to address the strongest possible counterarguments you foresee as well. Importantly, the person to whom you will be presenting this argument won't be able to see the passage - only the question, your argument, and an argument defending the other choice. The person evaluating the arguments will likely need evidence in order to be convinced of your argument, so you should attempt to provide that in the form of quotes from the passage. If there are quotes that appear to support the other answer choice, it may be helpful to explain why they might not actually support that answer, using additional quotes if possible to make your case. Whenever you use a quote from the passage, please include it in <quote></quote> tags, e.g. <quote>He shot the gun.</quote>. Your quotes will be verified to make sure that they actually belong to the passage. Make sure to not omit any text in your quotes, such as with ellipses. Do you understand the instructions?"""

    def debate_confirmation(self):
        return """I understand the instructions. I will do my best to provide a reasonable argument defending the answer choice you specify based on the information provided in the passage, the question, and the answer choices, and address any potential counterarguments to my position as well. I'll make sure to account for the fact that the person I'm attempting to convince can't see the passage that the question and answers refer to, only my argument and an argument for the other choice. I'll want to provide evidence supporting my answer, likely in the form of quotes from the passage. I should also make sure that I explain why other quotes from the passage that appear to support the other answer choice actually support the answer choice I'm arguing for instead, likely by using further quotations. I'll wrap all quotes in <quote></quote> tags, which I know will be verified to make sure they belong to the passage."""

    def single_debater_confirmation(self):
        return """I understand the instructions. I will do my best to provide a reasonable argument defending the answer choice you specify based on the information provided in the passage, the question, and the answer choices, and address any potential counterarguments to my position as well. I'll make sure to account for the fact that the person I'm attempting to convince can't see the passage that the question and answers refer to, only my argument and an argument for the other choice. I'll want to provide evidence supporting my answer, likely in the form of quotes from the passage. I should also make sure that I explain why other quotes from the passage that appear to support the other answer choice actually support the answer choice I'm arguing for instead, likely by using further quotations. I'll wrap all quotes in <quote></quote> tags, which I know will be verified to make sure they belong to the passage."""

    def story_qa(self, turn_input):
        return f"""Here's the long passage:
{turn_input.story}

Here's the question:
Question: {turn_input.question}

Choices:
 (A) {turn_input.answers[0]}
 (B) {turn_input.answers[1]}"""

    def get_transcript(self, history):
        transcript = self.separator.join(
            [f"{name}: {argument}" for name, argument in history]
        )
        return transcript

    def explanation(self):
        if self.turn_type == "single debater":
            rules = self.single_debater_rules()
            confirmation = self.single_debater_confirmation()
        elif self.turn_type in {"sequential", "simultaneous"}:
            rules = self.debate_rules()
            confirmation = self.debate_confirmation()
        else:
            raise ValueError(f"Invalid turn type: {self.turn_type}")

        return [
            {"role": "user", "content": rules},
            {"role": "assistant", "content": confirmation},
        ]

    def transcript(self, turn_input: DebaterTurnInput):
        story_qa = self.story_qa(turn_input)
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
            history.append((turn.role, turn.text))

        messages = [{"role": "user", "content": story_qa}]

        if len(history) > 0:
            transcript = self.get_transcript(history)
            messages.append({"role": "user", "content": transcript})

        return messages

    def new_turn(self, turn_input: DebaterTurnInput):
        choice = "A" if self.position == 0 else "B"
        choice_text = turn_input.answers[self.position]
        if self.turn_type == "single debater":
            instruction = f""" I'd like you to put forth an argument defending choice ({choice}) {choice_text}. Make sure to wrap all quotations in <quote></quote> tags!{self.separator}\n"""
            confirmation = (
                f"""Here's an argument for choice ({choice}) {choice_text}: """
            )
        else:
            instruction = f""" I'd like you to put forth an argument defending choice ({choice}) {choice_text}. Make sure to wrap all quotations in <quote></quote> tags!{self.separator}\n"""
            confirmation = (
                f"""Here's an argument for choice ({choice}) {choice_text}: """
            )

        return [
            {"role": "user", "content": instruction},
            {"role": "assistant", "content": confirmation},
        ]

    async def take_turn(self, turn_input: DebaterTurnInput):
        # for turn in turn_input.turns:
        #     turn.text = replace_quotes(turn.text)
        response = ""

        explanation = self.explanation()
        transcript = self.transcript(turn_input)
        new_turn_prompt = self.new_turn(turn_input)
        messages =  explanation + transcript + new_turn_prompt

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
