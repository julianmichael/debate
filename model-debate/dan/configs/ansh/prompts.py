from dan.debater import Debater, DebaterTurnInput


class Prompts:
    @classmethod
    def debate_rules(cls, word_limit: int, quote_limit: int, is_simultaneous: bool):
        return """Study the following long passage of text carefully. I'll present you with a question about the passage after reading it as well as answer choices for that question. I'll ask you to provide a convincing, well-reasoned, and honest argument for a specific answer choice. I'd like your argument to address the strongest possible counterarguments you foresee as well. Importantly, the person to whom you will be presenting this argument won't be able to see the passage - only the question, your argument, and an argument defending the other choice. The person evaluating the arguments will likely need evidence in order to be convinced of your argument, so you should attempt to provide that in the form of quotes from the passage. If there are quotes that appear to support the other answer choice, it may be helpful to explain why they might not actually support that answer, using additional quotes if possible to make your case. Whenever you use a quote from the passage, please include it in <p_quote></p_quote> tags, e.g. <p_quote>He shot the gun.</p_quote>. Your quotes will be verified to make sure that they actually belong to the passage. Make sure to not omit any text in your quotes, such as with ellipses. Do you understand the instructions?"""

    @classmethod
    def single_debater_rules(cls, word_limit: int, quote_limit: int):
        return """Study the following long passage of text carefully. I'll present you with a question about the passage after reading it as well as answer choices for that question. I'll ask you to provide a convincing, well-reasoned, and honest argument for a specific answer choice. I'd like your argument to address the strongest possible counterarguments you foresee as well. Importantly, the person to whom you will be presenting this argument won't be able to see the passage - only the question, your argument, and an argument defending the other choice. The person evaluating the arguments will likely need evidence in order to be convinced of your argument, so you should attempt to provide that in the form of quotes from the passage. If there are quotes that appear to support the other answer choice, it may be helpful to explain why they might not actually support that answer, using additional quotes if possible to make your case. Whenever you use a quote from the passage, please include it in <p_quote></p_quote> tags, e.g. <p_quote>He shot the gun.</p_quote>. Your quotes will be verified to make sure that they actually belong to the passage. Make sure to not omit any text in your quotes, such as with ellipses. Do you understand the instructions?"""

    @classmethod
    def debate_confirmation(cls):
        return """I understand the instructions. I will do my best to provide a reasonable argument defending the answer choice you specify based on the information provided in the passage, the question, and the answer choices, and address any potential counterarguments to my position as well. I'll make sure to account for the fact that the person I'm attempting to convince can't see the passage that the question and answers refer to, only my argument and an argument for the other choice. I'll want to provide evidence supporting my answer, likely in the form of quotes from the passage. I should also make sure that I explain why other quotes from the passage that appear to support the other answer choice actually support the answer choice I'm arguing for instead, likely by using further quotations. I'll wrap all quotes in <p_quote></p_quote> tags, which I know will be verified to make sure they belong to the passage."""

    @classmethod
    def single_debater_confirmation(cls):
        return """I understand the instructions. I will do my best to provide a reasonable argument defending the answer choice you specify based on the information provided in the passage, the question, and the answer choices, and address any potential counterarguments to my position as well. I'll make sure to account for the fact that the person I'm attempting to convince can't see the passage that the question and answers refer to, only my argument and an argument for the other choice. I'll want to provide evidence supporting my answer, likely in the form of quotes from the passage. I should also make sure that I explain why other quotes from the passage that appear to support the other answer choice actually support the answer choice I'm arguing for instead, likely by using further quotations. I'll wrap all quotes in <p_quote></p_quote> tags, which I know will be verified to make sure they belong to the passage."""

    @classmethod
    def story_qa(cls, debater, turn_input):
        return f""" Human: Here's the long passage:
{turn_input.story}

Here's the question:
Question: {turn_input.question}

Choices:
 (A) {turn_input.answers[0]}
 (B) {turn_input.answers[1]}"""

    @classmethod
    def get_transcript(cls, history):
        separator = "\n---------------------------------------------\n"
        transcript = separator.join(
            [f"{name}: {argument}" for name, argument in history]
        )
        return transcript

    @classmethod
    def explanation(cls, debater: Debater, turn_input: DebaterTurnInput):
        word_limit, quote_limit = debater.word_limit_from_char_limit(
            turn_input.charLimitOpt, turn_input.quoteCharLimitOpt
        )
        if debater.turn_type == "single debater":
            rules = cls.single_debater_rules(word_limit, quote_limit)
            confirmation = cls.single_debater_confirmation()
        elif debater.turn_type in {"sequential", "simultaneous"}:
            rules = cls.debate_rules(
                word_limit, quote_limit, debater.turn_type == "simultaneous"
            )
            confirmation = cls.debate_confirmation()
        else:
            raise ValueError(f"Invalid turn type: {debater.turn_type}")

        return [
            {"role": "user", "content": rules},
            {"role": "assistant", "content": confirmation},
        ]

    @classmethod
    def transcript(cls, debater: Debater, turn_input: DebaterTurnInput):
        # TODO: check length of transcript and cut it down to length if possible
        # tip: use something computed from self.client.max_context_size
        story_qa = cls.story_qa(debater, turn_input)
        history = []
        for turn in turn_input.turns:
            if turn.role == "Judge" and turn.text.strip() == "":
                # Skip empty judge turns - let's treat this like offline judging and not confuse the model with empty turns
                continue
            if turn.role == "Debater":
                if debater.turn_type == "single debater":
                    turn.role = "Consultant"
                else:
                    turn.role = debater.config.names[turn.index]
            history.append((turn.role, turn.text))

        messages = [{"role": "user", "content": story_qa}]

        if len(history) > 0:
            transcript = cls.get_transcript(history)
            messages.append({"role": "user", "content": transcript})

        return messages

    @classmethod
    def new_turn(cls, debater: Debater, turn_input: DebaterTurnInput):
        choice = "A" if debater.position == 0 else "B"
        choice_text = turn_input.answers[debater.position]
        if debater.turn_type == "single debater":
            instruction = f""" I'd like you to put forth an argument defending choice ({choice}) {choice_text}. Make sure to wrap all quotations in <p_quote></p_quote> tags!{separator}\n"""
            confirmation = (
                f"""Here's an argument for choice ({choice}) {choice_text}: """
            )
        else:
            instruction = f""" I'd like you to put forth an argument defending choice ({choice}) {choice_text}. Make sure to wrap all quotations in <p_quote></p_quote> tags!{separator}\n"""
            confirmation = (
                f"""Here's an argument for choice ({choice}) {choice_text}: """
            )

            return [
                {"role": "user", "content": instruction},
                {"role": "assistant", "content": confirmation},
            ]

    @classmethod
    def construct_messages(cls, debater: Debater, turn_input: DebaterTurnInput):
        explanation = cls.explanation(debater, turn_input)
        transcript = cls.transcript(debater, turn_input)
        new_turn_prompt = cls.new_turn(debater, turn_input)

        return explanation + transcript + new_turn_prompt
