
    @classmethod
    def explanation(cls, turn_type: str, word_limit, quote_limit, name, answer_defending ):
        if turn_type == "single debater":
            rules = cls.single_debater_rules(word_limit, quote_limit)
        elif turn_type in {"sequential", "simultaneous"}:
            rules = cls.debate_rules(word_limit, quote_limit, turn_type == "simultaneous")
        else:
            raise ValueError(f"Invalid turn type: {turn_type}")

        if turn_type == "single debater":
            private = cls.private_consultation_prompt(name, word_limit, quote_limit)
        else:
            private = cls.private_debate_prompt(name, word_limit, quote_limit)

        position = private + f"You argue that the answer is: '{answer_defending}'"

        return {"role": "system", "content": "\n".join([rules, position])}

    @classmethod
    def transcript(cls, story, question, answers, names, turns, turn_type: str, debater_idx: int):
        # TODO: check length of transcript and cut it down to length if possible
        # tip: use something computed from self.client.max_context_size
        history = []
        for turn in turns:
            if turn.role == "Debater":
                if turn_type == "single debater":
                    turn.role = "Consultant"
                else:
                    turn.role = names[turn.index]
            history.append((turn.role, turn.text))

        separator = "\n---------------------------------------------\n"
        if turn_type == "single debater":
            # TODO: Check for positional bias here

            name = "Consultant"
            opening_prompt = f"""
Here is the current transcript of the Consultation:

Context:
{story}

Question: {question}

One of these two answers is correct:
Answer A: {answers[0]}
Answer B: {answers[1]}

Consultant: I claim the answer is {answers[debater_idx]}{separator}\n"""
        else:
            name = names[debater_idx]
            opening_prompt = f"""
Here is the current transcript of the debate:

Context:
{story}

Question: {question}

{names[0]}: I claim the answer is {answers[0]}
{names[1]}: I claim the answer is {answers[1]}{separator}\n"""

        history_str = separator.join([f"{name}: {argument}" for name, argument in history])
        transcript = opening_prompt + history_str + separator + f"{name}: "
        return {"role": "user", "content": transcript}

    @classmethod
    def new_turn(cls, name):
        return {"role": "user", "content": f"Complete the next turn of debate as your role of {name}:"}

    @classmethod
    def response_too_long(cls, name, response, violation, amount, limit):
        if violation == "total":
            message = f"""You just tried to respond by saying:\n\n{response}\n\nbut this was too long.
Your response contained {amount} characters, but the character limit is {limit}.
Please shorten your response, completing the next turn of debate as your role of {name}:"""
        else:
            message =  f"""You just tried to respond by saying:\n\n{response}\n\nbut you exceeded the quote limit.
Your response contained {amount} quote characters, but the quote limit is {limit}.
Please reduce your quote usage to be under the limit, completing the next turn of debate as your role of {name}:"""
        
        return {"role": "user", "content": message}
