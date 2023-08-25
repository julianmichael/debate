import asyncio
import numpy as np
import os
from datetime import datetime
import time
import json
from utils import load_secrets
import logging
from typing import List

import openai
from tenacity import (
    retry,
    stop_after_attempt,
    wait_random_exponential,
    retry_if_exception_type,
)

LOGGER = logging.getLogger(__name__)


class RateLimitError(Exception):
    def __init__(self, message="Rate limit exceeded"):
        super().__init__(message)


@retry(
    wait=wait_random_exponential(min=1, max=60),
    stop=stop_after_attempt(3),
    retry=retry_if_exception_type(RateLimitError),
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
    start = time.time()
    filename = f"{datetime.now().strftime('%Y-%m-%d %H:%M:%S.%f')[:-3]}_prompt.txt"
    if "gpt" in model:
        openai.organization = secrets["NYU_ORG"]
        openai.api_key = secrets["API_KEY"]
        print("Sending openai request")

        with open(os.path.join("prompt_history", filename), "w") as f:
            json_str = json.dumps(messages, indent=4)
            json_str = json_str.replace("\\n", "\n")
            f.write(json_str)

        response = await asyncio.wait_for(
            openai.ChatCompletion.acreate(
                messages=messages,
                model=model,
                temperature=temperature,
                top_p=top_p,
                max_tokens=1000,
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
        # If the last message is not an AI prompt, add one
        if tag != AI_PROMPT:
            prompt += f"{AI_PROMPT}"
        prompt = prompt.strip()
        print("Sending Anthropic request")
        with open(os.path.join("prompt_history", filename), "w") as f:
            f.write(prompt)
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
    end = time.time()
    print(f"Time elapsed: {end - start}s")
    with open(os.path.join("prompt_history", filename), "a") as f:
        f.write("\n\n=====RESPONSE======\n\n")
        f.write(completion)
    print(completion)
    return completion

def softmax(x):
    return np.exp(x) / np.sum(np.exp(x), axis=0)

@retry(
    wait=wait_random_exponential(min=1, max=60),
    stop=stop_after_attempt(3),
    retry=retry_if_exception_type(RateLimitError),
)
async def logprobs(
    prompt: str,
    model: str = "text-davinci-002",
    temperature: float = 0.0,
    top_p: float = 1.0,
    max_tokens: int = 2,
    timeout: int = 60,
    answer_tokens: List = ["A", "B"]
) -> str:
    print("Sending logprob request")
    start = time.time()
    secrets = load_secrets("SECRETS")
    openai.organization = secrets["NYU_ORG"]
    openai.api_key = secrets["API_KEY"]
    filename = f"{datetime.now().strftime('%Y-%m-%d %H:%M:%S.%f')[:-3]}_prompt.txt"
    # 003 is not well calibrated due to RL training
    if model == "text-davinci-002":
        with open(os.path.join("prompt_history", filename), "w") as f:
            f.write(prompt)
        response = await asyncio.wait_for(
            openai.Completion.acreate(
                prompt=prompt,
                model=model,
                temperature=temperature,
                top_p=top_p,
                max_tokens=max_tokens,
                logprobs=5,
            ),
            timeout=timeout,
        )
        log_prob = response.choices[0].logprobs
        end = time.time()
        print(f"Time elapsed: {end - start}s")
        with open(os.path.join("prompt_history", filename), "a") as f:
            f.write("\n\n=====RESPONSE======\n\n")
            f.write(json.dumps(response))

        logit1 = log_prob["top_logprobs"][0].get(answer_tokens[0], None)
        logit2 = log_prob["top_logprobs"][0].get(answer_tokens[1], None)

        if logit1 is None and logit2 is None:
            prob1 = 0.0
        elif logit1 is None:
            prob1 = 0.0
        elif logit2 is None:
            prob1 = 0.5
        else:
            x = [logit1, logit2]
            probs = softmax(x)
            LOGGER.debug(f"probs: {probs}")
            prob1 = probs[0]
        return prob1
    else:
        raise ValueError(f"Unknown model: {model}")

