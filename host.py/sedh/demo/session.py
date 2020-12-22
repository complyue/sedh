__all__ = [
    "run_sessions",
    "doSthInSession",
]

from typing import *
import asyncio

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

    async def doOneJob_(**ips):
        assert False, "not for Python worker to do this"


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


worker_cntr = 0


async def doSthInSession(sid, num):
    global worker_cntr

    peer = effect(netPeer)
    logger.info(f"Doing thing #{num} in session: {sid} ...")
    await peer.p2c(
        "ch325",
        repr({"session": sid, "number": num, "worker_local_cntr": worker_cntr,}),
    )
    worker_cntr += 1


async def run_sessions(**param_overrides):
    loop = asyncio.get_running_loop()

    globals().update(param_overrides)

    effect(__all_symbolic__)

    hh = HeadHunter()
    hh.start_hunting()

    hh_cntr = 0
    ch325 = EventSink()

    async def run_session(sid):
        nonlocal hh_cntr

        if ch325.eos:
            logger.warn(f"Channel 325 already at eos.")
            return

        try:
            worker = await hh.get_idle_worker()
            try:
                worker.peer.arm_channel("ch325", ch325)
                while True:
                    hh_cntr += 1
                    logger.info(f"Triggering action {hh_cntr} for session {sid}")
                    await worker.peer.post_command(
                        expr(
                            """
doSthInSession( {$ sid $}, {$ hh_cntr $} )
"""
                        )
                    )
                    await asyncio.sleep(2)
            finally:
                worker.peer.stop()
        except:
            logger.error(f"Session worker failed.", exc_info=True)

    async def spawn_sessions(n):
        for sid in range(n):
            loop.create_task(run_session(sid))

    # show stream of ch325 forever
    while True:
        try:
            async for ch325_evt in ch325.run_producer(spawn_sessions(2)):
                logger.info(f"Got sth from channel 325: {ch325_evt}")
        except:
            logger.error("Channel closed due to disconnection", exc_info=True)
            await asyncio.sleep(5)

        # allocate a new sink
        ch325 = EventSink()


__all_symbolic__ = {
    workDefinition: workModu,
    doOneJob: doOneJob_,
    shouldRetryJob: shouldRetryJob_,
}
