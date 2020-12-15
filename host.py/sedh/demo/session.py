__all__ = [
    "run_sessions",
    "doSthInSession",
]

from typing import *
import asyncio

import time

from edh import *
from nedh import *
from sedh import *

# work definition scripts are allowed to change the inferred configuration at
# `sedh.senv`, import the module as a reference, to get/set effective
# artifacts living there
import sedh.senv

from .etc import *


logger = sedh.log.get_logger(__name__)


# `sedh.senv` module controls how the worker processes are to be launched,
# then how each of those processes will locate the actual job computation code

use_haskell_workers = False
if use_haskell_workers:

    # to invoke following Haskell based work from a Python based HH
    sedh.senv.jobExecutable = [
        "epm",
        "x",
        "gwd",
    ]
    workModu = "swarm/demo/session"


else:

    # the enclosing module here is the Python based work definition module
    workModu = __name__

    async def doOneJob_(**ips):
        # this single job will never end until the nedh connection is disconnected
        peer = effect(netPeer)
        await peer.eol()


async def shouldRetryJob_(jobExc: object, ips: Dict):
    logger.error(f"Sessions failed: {jobExc}")
    return ips  # retry the same ips
    # a different ips can be returned from here to retry


async def doSthInSession(num):
    logger.info(f"Doing this thing in session: {num}")


async def run_sessions(**param_overrides):
    loop = asyncio.get_running_loop()

    globals().update(param_overrides)

    effect(__all_symbolic__)

    hh = HeadHunter()
    hh.start_hunting()

    async def run_session(sid):
        while True:
            try:
                worker = await hh.get_idle_worker()
                while True:
                    logger.info(f"Triggering session action {sid+101}")
                    await worker.peer.post_command(
                        expr(
                            """
doSthInSession( {$ sid+101 $} )
"""
                        )
                    )
                    await asyncio.sleep(2)
            except:
                logger.error(f"Session worker failed.", exc_info=True)

    for sid in range(2):
        loop.create_task(run_session(sid))

    # blocking wait here to prevent the main coroutine/thread from terminating
    await hh


__all_symbolic__ = {
    workDefinition: workModu,
    doOneJob: doOneJob_,
    shouldRetryJob: shouldRetryJob_,
}

