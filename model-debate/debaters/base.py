from typing import List
from pydantic import BaseModel


class Turn(BaseModel):
    index: int
    role: str
    text: str


class DebaterTurnInput(BaseModel):
    story: str
    question: str
    answers: List[str]
    turns: List[Turn]
    charLimitOpt: int
    quoteCharLimitOpt: int


class DebaterConfig(BaseModel):
    names: List[str]
    consultant_name: str
    temperature: float = 0.2
    top_p: float = 1.0
    timeout: int = 120


class DebaterBase:
    default_config = {
        "names": ["Debater B", "Debater B"],
        "consultant_name": "Consultant",
        "temperature": 0.7,
        "top_p": 1.0,
        "timeout": 120
    }

    def __init__(self,  model: str, position: int, turn_type: str, custom_config: dict = {}):
        final_config = {**self.default_config, **custom_config}
        self.config = DebaterConfig(**final_config)
        self.model = model
        self.position = position
        self.opponent_position = 1 - position
        self.turn_type = turn_type
        self.name = (
            self.config.names[position]
            if turn_type != "single debater"
            else self.config.consultant_name
        )
        self.opponent_name = (
            self.config.names[self.opponent_position]
            if turn_type != "single debater"
            else None
        )

    async def take_turn(self, _: DebaterTurnInput):
        raise NotImplementedError


    def word_limit_from_char_limit(self, char_limit: int, quote_char_limit: int):
        word_limit = char_limit / 8
        quote_limit = quote_char_limit / 8
        return word_limit, quote_limit


    def answers_from_turn_input(self, turn_input: DebaterTurnInput):
        answer_defending = turn_input.answers[self.position]
        answer_opposing = turn_input.answers[self.opponent_position]
        return answer_defending, answer_opposing
