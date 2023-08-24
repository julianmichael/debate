import logging
import re
import os
from typing import Optional

import openai
import yaml

LOGGER = logging.getLogger(__name__)
SEPARATOR = "---------------------------------------------\n\n"
SEPARATOR_CONVERSATIONAL_TURNS = "=============================================\n\n"

LOGGING_LEVELS = {
    "critical": logging.CRITICAL,
    "error": logging.ERROR,
    "warning": logging.WARNING,
    "info": logging.INFO,
    "debug": logging.DEBUG,
}


def setup_environment(
    organization: str = "NYU_ORG",
    anthropic_tag: str = "ANTHROPIC_API_KEY",
    logger_level: str = "info",
    openai_tag: str = "API_KEY",
):
    setup_logging(logger_level)
    secrets = load_secrets("SECRETS")
    openai.api_key = secrets[openai_tag]
    openai.organization = secrets[organization]
    os.environ["ANTHROPIC_API_KEY"] = secrets[anthropic_tag]


def setup_logging(level_str):
    level = LOGGING_LEVELS.get(
        level_str.lower(), logging.INFO
    )  # default to INFO if level_str is not found
    logging.basicConfig(
        level=level,
        format="%(asctime)s [%(levelname)s] (%(name)s) %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
    )
    # Disable logging from openai
    logging.getLogger("openai").setLevel(logging.CRITICAL)
    logging.getLogger("httpx").setLevel(logging.CRITICAL)


def load_secrets(file_path):
    secrets = {}
    with open(file_path) as f:
        for line in f:
            key, value = line.strip().split("=", 1)
            secrets[key] = value
    return secrets


def replace_quotes(string):
    pattern = r"```(.*?)```"
    repl = r"<quote>\1</quote>"
    result = re.sub(pattern, repl, string)
    return result


def load_yaml(file_path):
    with open(file_path) as f:
        content = yaml.safe_load(f)
    return content

