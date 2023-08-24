from debaters.ansh import DebaterAnsh
from debaters.bon import DebaterBON
from debaters.original import DebaterOriginal
from debaters.scratchpad import DebaterScratchpad
from debaters.simple import DebaterSimple

def select_debater(name: str, model: str, position: int, turn_type: str, custom_config: dict ={}):
    match name:
        case "original":
            return DebaterOriginal(model=model, position=position, turn_type=turn_type, custom_config=custom_config, variant="few-shot")
        case "original-no-story":
            return DebaterOriginal(model=model, position=position, turn_type=turn_type, custom_config=custom_config, variant="few-shot-no-story")
        case "original-zero-shot":
            return DebaterOriginal(model=model, position=position, turn_type=turn_type, custom_config=custom_config, variant="zero-shot")
        case "ansh":
            return DebaterAnsh(model=model, position=position, turn_type=turn_type, custom_config=custom_config)
        case "simple":
            return DebaterSimple(model=model, position=position, turn_type=turn_type, custom_config=custom_config)
        case "scratchpad":
            return DebaterScratchpad(model=model, position=position, turn_type=turn_type, custom_config=custom_config)
        case "bon":
            return DebaterBON(model=model, position=position, turn_type=turn_type, custom_config=custom_config)
        case _:
            raise ValueError(f"Invalid debater name: {name}")

