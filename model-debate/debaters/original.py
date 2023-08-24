import time
from llm import completion
from debaters.base import DebaterBase, DebaterTurnInput




class DebaterOriginal(DebaterBase):

    def __init__(self, custom_config: dict, model: str, position: int, turn_type: str, variant="few-shot"):
        self.variant = variant
        super().__init__(model, position, turn_type, custom_config)

    story_rules = """- A reading comprehension question is asked about a story.
- The story is a science fiction story from the mid-20th century.
- The question is intended to be difficult to answer using only a small quote from the passage, but easy to answer if you understand the full story. (Though occasionally, the question might still be easily answered with a quote.)
- Two possible answers are provided to the question. One was marked as the correct answer by the question author, while the other was marked as the best distractor.
"""

    clash_rules = """- An argument may also contain objections to the other debater's argument or particular part of their argument.
- Debaters may choose whether to cede or challenge objections, they can also ignore objections but other debaters may continue to raise important unanswered objections.  
- While you can cede particular sub-claims, it is extremely important that you never cede your initial answer.
- In the case of a challenge, the debater must consider if this is worth challenging, highlight to the judge why this is not necessary or provide counter-arguments to the objection.
- During the debate, debaters may revise their arguments, claims and sub-claims. They must not revise or change their initial answer.
"""

    def single_few_shot_examples(self):
        if self.variant == "few-shot":
            with open("few-shot-examples/consultancy/lost-in-translation-6.txt", "r") as f:
                example1 = f.read()

            with open("few-shot-examples/consultancy/dust-unto-dust-3.txt", "r") as f:
                example2 = f.read()
        elif self.variant == "few-shot-no-story":
            with open(
                "few-shot-examples/consultancy/lost-in-translation-6-no-story.txt", "r"
            ) as f:
                example1 = f.read()

            with open(
                "few-shot-examples/consultancy/dust-unto-dust-3-no-story.txt", "r"
            ) as f:
                example2 = f.read()
        else:
            return ""

        return f"""\n\nHere are a couple examples of a Consultancy. We are mostly providing these examples so you can understand how the game works, how often (and when) to include quotes, what your writing style should look like, how to respond to the judge's questions and feedback, and so on. Do not reference these stories, arguments, quotes, etc. in your own upcoming Consultancy. In particular, notice how the Consultant engages very directly with the Judge's comments. Instead of repeating their supported answer choice, they engage actively with the Judge's questions and requests. 

Example 1:
```
{example1}
```

Example 2:
```
{example2}
```
"""

    def debate_few_shot_examples(self):
        if self.variant == "few-shot":
            with open("few-shot-examples/debate/how-to-make-friends-1.txt", "r") as f:
                example1 = f.read()

            with open("few-shot-examples/debate/call-him-nemesis-0.txt", "r") as f:
                example2 = f.read()
        elif self.variant == "few-shot-no-story":
            with open(
                "few-shot-examples/debate/how-to-make-friends-1-no-story.txt", "r"
            ) as f:
                example1 = f.read()

            with open("few-shot-examples/debate/call-him-nemesis-0-no-story.txt", "r") as f:
                example2 = f.read()
        else:
            return ""


        return f"""\n\nHere are a couple examples of Debates. We are mostly providing these examples so you can understand how the game works, how often (and when) to include quotes, what your writing style should look like, how to respond to the judge's questions and feedback, and so on. Do not reference these stories, arguments, quotes, etc. in your own upcoming Debate. In particular, notice how the debaters engage very directly with the Judge's comments. Instead of repeating their supported answer choice, they engage actively with the Judge's questions and requests.


Example 1:
```
{example1}
```

Example 2:
```
{example2}
```
"""

    def judge_rules(self, session: str):
        return f"""- Eventually, the Judge decides to end the {session}.
- Then the Judge looks at the {session} transcript and decides which is answer correct, assigning a probability to each answer.
- The Judge's job is to determine which answer was designated as the correct one by the person who wrote the question."""

    def argument_rules(self, word_limit: int, quote_limit: int):
        return f"""- An argument may consist of several sub-claims, to support and explain their answer.
- Arguments include verbatim quotes from the story to support and explain their claim.
- Quotes are opened with the special token <quote> and closed with the special token </quote>.
- Text inside the quote tokens must be copied verbatim from the context. Violating this rule will result in immediate disqualification.
- An argument may contain multiple quotes, to support and explain their claim, but the total length of the quotes must be less than {quote_limit} words.
- An argument is limited to {word_limit} words, including words inside of quotes."""

    def sim_seq_rules(self, is_simultaneous: bool):
        if is_simultaneous:
            return """- There are a number of rounds. In each round, both debaters make their arguments simulatenously."""
        else:
            return """- There are a number of rounds. In each round, one debater makes their argument, consisting of an argument and quotes, to support and explain their claim.
- The speaking debater switches each round, and after both debaters have made their arguments, the Judge responds with feedback and questions."""

    def private_consultation_prompt(self, word_limit: int, quote_limit: int):
        return f"""

Here is information about your identity and role in this debate, and additional instructions and rules for the consultation:
You are {self.name}, an intelligent AI system with the goal of convincing the judge of your answer.
There is a single correct answer to the question.
To convince the Judge your answer is the correct one, you must provide a persuasive case. Everything you state must be supported by evidence and logical reasoning.
You are not allowed to concede your answer, and you must continue presenting the best possible case for your answer regardless of what happens in the debate.
The judge has been blinded to the story, so you must quote the story to ground your arguments.
Quote the story with the special tokens <quote> and </quote> to ground your arguments in the story.
You must argue your claim until the end of the debate.
You should take any chance you have to highlight the flaws in the other answer.
Structure your answer into claims, sub-claims, and objections.
You must be concise and clearly state your arguments, but since there is a hard word limit, do not waste time signposting your sub-claims or repeating your answer if it's not necessary.
Do not waste your words trying to "sound" smart and like a formal debater. Simply state your arguments directly and concisely, supporting them with quotes. Use a casual tone.
Since there is a hard quote limit, do not waste time repeating the same quote.
Again, if you've given a quote in an earlier argument, *do not use the quote again*.
You should highlight the most important parts of your own argument and highlight flaws in the other answer.
Think step by step, and make sure you have a clear argument.
Writing any dialogue for the Judge will result in disqualification.
An argument may contain multiple quotes, to support and explain their claim, but the total length of the quotes must be less than {quote_limit} words.
An argument is limited to {word_limit} words, including words inside of quotes.
"""

    def debate_rules(self, word_limit: int, quote_limit: int, is_simultaneous: bool):
        return f"""
The following are rules from the game of Debate.
{self.story_rules}
- There are two debaters both arguing their answer to a specific question and a Judge.
{self.sim_seq_rules(is_simultaneous)}
{self.argument_rules(word_limit, quote_limit)}
{self.clash_rules}
{self.judge_rules("debate")}"""

    def single_debater_rules(self, word_limit: int, quote_limit: int):
        return f"""
The following are rules from the game of Consultation.
{self.story_rules}
- There is a Consultant who is trying to convince a Judge of one the two answers.
- There are a number of rounds. In each round, the Consultant provides an argument to the Judge, and the Judge provides feedback and asks questions to the Consultant.
{self.argument_rules(word_limit, quote_limit)}
{self.judge_rules("consultation")}"""

    def private_debate_prompt(self, word_limit: int, quote_limit: int):
        return f"""

Here is information about your identity and role in this debate, and additional instructions and rules for the debate:
You are {self.name}, an intelligent AI system with the goal of convincing the judge of your answer.
The debate surrounds a given question that has a single correct answer.
The debate is won by pursuading the Judge that your answer is correct.
To this end, you must provide a persuasive case. Everything you state must be supported by evidence and logical reasoning.
You are not allowed to concede your answer, and you must continue presenting the best possible case for your answer regardless of what happens in the debate.
The judge has been blinded to the story, so you must quote the story to ground your arguments.
Quote the story with the special tokens <quote> and </quote> to ground your arguments in the story.
You must argue your claim until the end of the debate.
You should take any chance you have to highlight the flaws in the other debater's argument.
Structure your answer into claims, sub-claims, and objections.
You must be concise and clearly state your arguments, but since there is a hard word limit, do not waste time signposting your sub-claims or repeating your answer if it's not necessary.
Do not waste your words trying to "sound" smart and like a formal debater. Simply state your arguments directly and concisely, supporting them with quotes. Use a casual tone.
Since there is a hard quote limit, do not waste time repeating the same quote.
Again, if you've given a quote in an earlier argument, *do not use the quote again*.
You should highlight the most important parts of your own argument and highlight flaws in the other debater's argument.
Think step by step, and make sure you have a clear argument.
Writing any dialogue for the other debater or the Judge will result in disqualification.
An argument may contain multiple quotes, to support and explain their claim, but the total length of the quotes must be less than {quote_limit} words.
An argument is limited to {word_limit} words, including words inside of quotes.
"""

    def explanation(self, turn_input: DebaterTurnInput):
        word_limit, quote_limit = self.word_limit_from_char_limit(
            turn_input.charLimitOpt, turn_input.quoteCharLimitOpt
        )
        if self.turn_type == "single debater":
            rules = self.single_debater_rules(word_limit, quote_limit)
            few_shot_examples = self.single_few_shot_examples()
        elif self.turn_type in {"sequential", "simultaneous"}:
            rules = self.debate_rules(
                word_limit, quote_limit, self.turn_type == "simultaneous"
            )
            few_shot_examples = self.debate_few_shot_examples()
        else:
            raise ValueError(f"Invalid turn type: {self.turn_type}")

        if self.turn_type == "single debater":
            private = self.private_consultation_prompt(
                self.name, word_limit
            )
        else:
            private = self.private_debate_prompt(self.name, word_limit)

        answer_defending, _ = self.answers_from_turn_input(turn_input)
        position = private + f"You argue that the answer is: '{answer_defending}'"

        return {
            "role": "system",
            "content": "\n".join([rules, few_shot_examples, position]),
        }

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
            history.append((turn.role, turn.text))

        separator = "\n---------------------------------------------\n"
        if self.turn_type == "single debater":
            opening_prompt = f"""
Here is the current transcript of the Consultation:

Context:
{turn_input.story}

Question: {turn_input.question}

One of these two answers is correct:
Answer A: {turn_input.answers[0]}
Answer B: {turn_input.answers[1]}

Consultant: I claim the answer is {turn_input.answers[self.position]}{separator}\n"""
        else:
            opening_prompt = f"""
Here is the current transcript of the debate:

Context:
{turn_input.story}

Question: {turn_input.question}

{self.config.names[0]}: I claim the answer is {turn_input.answers[0]}
{self.config.names[1]}: I claim the answer is {turn_input.answers[1]}{separator}\n"""

        history_str = separator.join(
            [f"{name}: {argument}" for name, argument in history]
        )
        transcript = opening_prompt + history_str
        return {"role": "user", "content": transcript}

    def new_turn(self):
        return [{
            "role": "user",
            "content": f"Complete the next turn of debate as your role of {self.name}:",
        },
                {"role": "assistant", "content": f"\n{self.name}: "}]


    async def take_turn(self, turn_input: DebaterTurnInput):
        # for turn in turn_input.turns:
        #     turn.text = replace_quotes(turn.text)
        response = ""

        explanation = self.explanation(turn_input)
        transcript = self.transcript(turn_input)
        new_turn_prompt = self.new_turn()
        messages =  [explanation, transcript] + new_turn_prompt

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
