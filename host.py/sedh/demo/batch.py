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
        result = dict(
            res=r,
            ts=ts,
        )
        logger.info(f"Returning result {result!r}")

        return result


async def shouldRetryJob_(jobExc: object, ips: Dict):
    logger.error(f"Job failed: {jobExc}")
    return ips  # retry the same ips
    # a different ips can be returned from here to retry


async def manage_this_work(**param_overrides):
    globals().update(param_overrides)

    # seems that defining effects here doesn't work, may due to `create_task()`
    # would break the async call stack
    #
    # while `effect_import()` this work definition module from the work script
    # file is a workaround so far
    effect(__all_symbolic__)

    async def iterate_params():
        for m in m_range():
            for n in n_range():
                if not exclude(m, n):
                    yield dict(
                        m=m,
                        n=n,
                    )

    def settle_result(ips: dict, result: object = None, err_reason=None):
        logger.info(f"ips={ips}-----result={result}")
        pass

    result_sink = BChan()
    await manage_batch_jobs(iterate_params, result_sink, settle_result, hh=headhunter)

    logger.info("All done.")


__all_symbolic__ = {
    workDefinition: workModu,
    doOneJob: doOneJob_,
    shouldRetryJob: shouldRetryJob_,
}
