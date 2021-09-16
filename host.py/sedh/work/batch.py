"""
Swarm batch work manager

"""
__all__ = [
    "manage_batch_jobs",
]

from typing import *
import asyncio

from edh import *

from ..headhunter import *
from .. import log

logger = log.get_logger(__name__)


async def manage_batch_jobs(
    params: Callable[[], Iterable[Dict]], outlet: EventSink, settle_result: Callable,
):
    try:
        hh = HeadHunter(outlet, settle_result=settle_result)
        hh.start_hunting()
        async for ips in params():
            logger.debug(f"Dispatching job ips={ips!r}")
            await hh.dispatch_job(ips)
        logger.info("All jobs sent out.")
        await hh.finish_up()
        hh.stop()
    finally:
        outlet.publish(EndOfStream)
