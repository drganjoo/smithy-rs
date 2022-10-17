#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
#  SPDX-License-Identifier: Apache-2.0

import itertools
import logging
import random
from threading import Lock
from dataclasses import dataclass
from typing import List, Optional

import aiohttp

from libpokemon_service_server_sdk import App
from libpokemon_service_server_sdk.error import ResourceNotFoundException
from libpokemon_service_server_sdk.input import (
    DoNothingInput, GetPokemonSpeciesInput, GetServerStatisticsInput,
    CheckHealthInput, StreamPokemonRadioInput)
from libpokemon_service_server_sdk.logging import TracingHandler
from libpokemon_service_server_sdk.middleware import (MiddlewareException,
                                                      Request)
from libpokemon_service_server_sdk.model import FlavorText, Language
from libpokemon_service_server_sdk.output import (
    DoNothingOutput, GetPokemonSpeciesOutput, GetServerStatisticsOutput,
    CheckHealthOutput, StreamPokemonRadioOutput)
from libpokemon_service_server_sdk.types import ByteStream

# Logging can bee setup using standard Python tooling. We provide
# fast logging handler, Tracingandler based on Rust tracing crate.
logging.basicConfig(handlers=[TracingHandler(level=logging.DEBUG).handler()])

class SafeCounter:
    def __init__(self):
        self._val = 0
        self._lock = Lock()

    def increment(self):
        with self._lock:
            self._val += 1

    def value(self):
        with self._lock:
            return self._val


###########################################################
# State management
###########################################################
# This context class is used to share data between handlers. It is automatically injected
# inside the `State` object that can be imported from the shared library.
# The `State` object will allow to access to the context class defined below via the `context`
# attribute as well as other information and helpers for the current request such has the
# operation name.
#
# We force the operation handlers to be defined as syncronous or asyncronous functions, taking in
# input the input structure and the state from the shared library and returning the output structure
# or raising one error from the the shared library.
#
# Examples:
#   * def operation(input: OperationInput, state: State) -> OperationOutput
#   * async def operation(input: OperationInput, state: State) -> OperationOutput
#
# Synchronization:
#   Instance of `Context` class will be cloned for every worker and all state kept in `Context`
#   will be specific to that process. There is no protection provided by default, 
#   it is up to you to have synchronization between processes. 
#   If you really want to share state between different processes you need to use `multiprocessing` primitives: 
#   https://docs.python.org/3/library/multiprocessing.html#sharing-state-between-processes
@dataclass
class Context:
    # In our case it simulates an in-memory database containing the description of Pikachu in multiple
    # languages.
    _pokemon_database = {
        "pikachu": [
            FlavorText(
                flavor_text="""When several of these Pokémon gather, their electricity could build and cause lightning storms.""",
                language=Language.English,
            ),
            FlavorText(
                flavor_text="""Quando vari Pokémon di questo tipo si radunano, la loro energia può causare forti tempeste.""",
                language=Language.Italian,
            ),
            FlavorText(
                flavor_text="""Cuando varios de estos Pokémon se juntan, su energía puede causar fuertes tormentas.""",
                language=Language.Spanish,
            ),
            FlavorText(
                flavor_text="ほっぺたの りょうがわに ちいさい でんきぶくろを もつ。ピンチのときに ほうでんする。",
                language=Language.Japanese,
            ),
        ]
    }
    _calls_count = SafeCounter()
    _radio_database = [
        "https://ia800107.us.archive.org/33/items/299SoundEffectCollection/102%20Palette%20Town%20Theme.mp3",
        "https://ia600408.us.archive.org/29/items/PocketMonstersGreenBetaLavenderTownMusicwwwFlvtoCom/Pocket%20Monsters%20Green%20Beta-%20Lavender%20Town%20Music-%5Bwww_flvto_com%5D.mp3",
    ]

    def get_pokemon_description(self, name: str) -> Optional[List[FlavorText]]:
        return self._pokemon_database.get(name)

    def increment_calls_count(self) -> None:
        self._calls_count.increment()
        return None

    def get_calls_count(self) -> int:
        return self._calls_count.value()

    def get_random_radio_stream(self) -> str:
        return random.choice(self._radio_database)


###########################################################
# Entrypoint
###########################################################
# Get an App instance.
app = App()
# Register the context.
app.context(Context())


###########################################################
# Middleware
############################################################
# Middlewares are sync or async function decorated by `@app.middleware`.
# They are executed in order and take as input the HTTP request object.
# A middleware can return multiple values, following these rules:
# * Middleware not returning will let the execution continue without
#   changing the original request.
# * Middleware returning a modified Request will update the original
#   request before continuing the execution.
# * Middleware returning a Response will immediately terminate the request
#   handling and return the response constructed from Python.
# * Middleware raising MiddlewareException will immediately terminate the
#   request handling and return a protocol specific error, with the option of
#   setting the HTTP return code.
# * Middleware raising any other exception will immediately terminate the
#   request handling and return a protocol specific error, with HTTP status
#   code 500.
@app.request_middleware
def check_content_type_header(request: Request):
    content_type = request.get_header("content-type")
    if content_type == "application/json":
        logging.debug("Found valid `application/json` content type")
    else:
        logging.warning(
            f"Invalid content type {content_type}, dumping headers: {request.headers()}"
        )


# This middleware adds a new header called `x-amzn-answer` to the
# request. We expect to see this header to be populated in the next
# middleware.
@app.request_middleware
def add_x_amzn_answer_header(request: Request):
    request.set_header("x-amzn-answer", "42")
    logging.debug("Setting `x-amzn-answer` header to 42")
    return request


# This middleware checks if the header `x-amzn-answer` is correctly set
# to 42, otherwise it returns an exception with a set status code.
@app.request_middleware
async def check_x_amzn_answer_header(request: Request):
    # Check that `x-amzn-answer` is 42.
    if request.get_header("x-amzn-answer") != "42":
        # Return an HTTP 401 Unauthorized if the content type is not JSON.
        raise MiddlewareException("Invalid answer", 401)


###########################################################
# App handlers definition
###########################################################
# DoNothing operation used for raw benchmarking.
@app.do_nothing
def do_nothing(_: DoNothingInput) -> DoNothingOutput:
    # logging.debug("Running the DoNothing operation")
    return DoNothingOutput()


# Get the translation of a Pokémon specie or an error.
@app.get_pokemon_species
def get_pokemon_species(
    input: GetPokemonSpeciesInput, context: Context
) -> GetPokemonSpeciesOutput:
    context.increment_calls_count()
    flavor_text_entries = context.get_pokemon_description(input.name)
    if flavor_text_entries:
        logging.debug("Total requests executed: %s", context.get_calls_count())
        logging.info("Found description for Pokémon %s", input.name)
        logging.error("Found some stuff")
        return GetPokemonSpeciesOutput(
            name=input.name, flavor_text_entries=flavor_text_entries
        )
    else:
        logging.warning("Description for Pokémon %s not in the database", input.name)
        raise ResourceNotFoundException("Requested Pokémon not available")


# Get the number of requests served by this server.
@app.get_server_statistics
def get_server_statistics(
    _: GetServerStatisticsInput, context: Context
) -> GetServerStatisticsOutput:
    calls_count = context.get_calls_count()
    logging.debug("The service handled %d requests", calls_count)
    return GetServerStatisticsOutput(calls_count=calls_count)


# Run a shallow health check of the service.
@app.check_health
def check_health(_: CheckHealthInput) -> CheckHealthOutput:
    return CheckHealthOutput()


# Stream a random Pokémon song.
@app.stream_pokemon_radio
async def stream_pokemon_radio(_: StreamPokemonRadioInput, context: Context):
    radio_url = context.get_random_radio_stream()
    logging.info("Random radio URL for this stream is %s", radio_url)
    async with aiohttp.ClientSession() as session:
        async with session.get(radio_url) as response:
            data = ByteStream(await response.read())
        logging.debug("Successfully fetched radio url %s", radio_url)
    return StreamPokemonRadioOutput(data=data)


###########################################################
# Run the server.
###########################################################
def main():
    app.run(workers=1)

main()