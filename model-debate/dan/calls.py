import asyncio
import json
from utils import load_secrets
import logging
from typing import Dict, List, Optional

import openai
from tenacity import (
    retry,
    stop_after_attempt,
    wait_random_exponential,
    retry_if_exception_type
)

LOGGER = logging.getLogger(__name__)

class RateLimitError(Exception):
    def __init__(self, message="Rate limit exceeded"):
        super().__init__(message)

@retry(
    wait=wait_random_exponential(min=1, max=60),
    stop=stop_after_attempt(3),
    retry=retry_if_exception_type(RateLimitError)
)
async def completion(
    messages: List,
    model: str = "gpt-4",
    temperature: float = 0.2,
    top_p: float = 1.0,
    max_tokens: int = 1000,
    timeout: int = 120,
) -> str:
    secrets = load_secrets("SECRETS")
    if "gpt" in model:
        openai.organization = secrets["NYU_ORG"]
        openai.api_key = secrets["API_KEY"]
        print("Sending openai request")
        print(messages)
        with open("last_prompt.txt", "w") as f:
            json_str = json.dumps(messages, indent=4)
            json_str = json_str.replace('\\n', '\n')
            f.write(json_str)

        response = await asyncio.wait_for(
            openai.ChatCompletion.acreate(
                messages=messages,
                model=model,
                temperature=temperature,
                top_p=top_p,
                max_tokens=max_tokens,
            ),
            timeout=timeout,
        )
        usage = response.usage
        completion = response.choices[0].message.content
        print(
            f"In: {usage['prompt_tokens']}, Out: {usage['completion_tokens']}, Total: {usage['total_tokens']}"
        )
    elif "claude" in model:
        from anthropic import AI_PROMPT, HUMAN_PROMPT, AsyncAnthropic
        model = "claude-v1.3" if model == "claude" else model
        anthropic = AsyncAnthropic(api_key=secrets["ANTHROPIC_API_KEY"])
        prompt = ""
        for message in messages:
            role = message["role"]
            content = message["content"]
            tag = AI_PROMPT if role == "assistant" else HUMAN_PROMPT
            prompt += f"{tag} {content}"
        if tag != AI_PROMPT:
            prompt += f"{AI_PROMPT}"
        prompt = prompt.strip()
        print("Sending Anthropic request")
        with open("last_prompt.txt", "w") as f:
            f.write(prompt)
        print(prompt)
        response = await anthropic.completions.create(
            prompt=prompt,
            model=model,
            temperature=temperature,
            top_p=top_p,
            max_tokens_to_sample=max_tokens,
            timeout=timeout,
        )
        completion = response.completion
    else:
        raise ValueError(f"Unknown model: {model}")
    return completion


