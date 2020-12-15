__all__ = [
    "manage_this_work",
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
    workModu = "swarm/demo/batch"

    async def doOneJob_(**ips):
        raise RuntimeError("Not for a Python worker to do this!")


else:

    # the enclosing module here is the Python based work definition module
    workModu = __name__

    # this should noramlly imported from more general modules
    async def compute_it(z):
        r = effect("c") * z
        return r

    m, n = 3, 7

    async def doOneJob_(**ips):
        globals().update(ips)

        effect(c=m + n)
        r = await compute_it(m * n)

        # simulate computation time cost, each taking 3 seconds.
        # await asyncio.sleep(3)
        ts = time.monotonic()

        # prepare the result to be returned
        result = dict(r=r, ts=ts,)
        logger.debug(f"Returning result {result!r}")

        return result


async def shouldRetryJob_(jobExc: object, ips: Dict):
    logger.error(f"Job failed: {jobExc}")
    return ips  # retry the same ips
    # a different ips can be returned from here to retry


async def manage_this_work(**param_overrides):
    globals().update(param_overrides)

    # seems that defining effects here doesn't work, may due to `create_task()`
    # in the `run_producer()` implementation breaks the async call stack
    #
    # while `effect_import()` this work definition module from the work script
    # file is a workaround so far
    effect(__all_symbolic__)

    def iter_params():
        for m in m_range():
            for n in n_range():
                if not exclude(m, n):
                    yield dict(
                        m=m, n=n,
                    )

    result_sink = EventSink()
    async for (ips, result) in result_sink.run_producer(
        manage_batch_jobs(iter_params, result_sink)
    ):
        logger.info(f"Job computed. ips={ips!r}, result={result!r}")

    logger.info("All done.")


__all_symbolic__ = {
    workDefinition: workModu,
    doOneJob: doOneJob_,
    shouldRetryJob: shouldRetryJob_,
}

