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
    params: Callable[[], Iterable[Dict]],
    outlet: BChan,
    settle_result: Callable,
    hh: HeadHunter = None,
):
    if hh is None:
        # 常规模式,即由发送端每次独立生成一个headhunter，发送完成后释放所有连接
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
            outlet.close()
    else:
        # 接力模式，headhunter由外部传入，即多任务之间不释放召集连接，反复复用
        hh._settle_result = settle_result
        async for ips in params():
            logger.debug(f"Dispatching job ips={ips!r}")
            await hh.dispatch_job(ips)
        await hh.finish_jobs()
        logger.info("All jobs sent out.")
