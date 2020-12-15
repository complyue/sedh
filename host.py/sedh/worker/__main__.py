import asyncio
import importlib
import inspect

from edh import *
from nedh import *
from sedh import *

# work definition scripts are allowed to change the inferred configuration at
# 'sedh.senv', import the module as a reference, to get/set effective artifacts
# living there
import sedh.senv


logger = sedh.log.get_logger(__package__)


async def _run_():
    loop = asyncio.get_running_loop()

    peer = await takeEdhFd(sedh.senv.wscFd)

    jobSink = peer.ensure_channel(DATA_CHAN)

    import nedh.effects

    effect_import(nedh.effects)

    effect(
        {netPeer: peer, dataSink: peer.ensure_channel(DATA_CHAN),}
    )

    # import the work definition module
    work_defi_modu = importlib.import_module(sedh.senv.jobWorkSpec)
    # obtain doOneJob
    doOneJob_ = work_defi_modu.__all_symbolic__[doOneJob]
    locals().update(
        # import all exposed artifacts into local scope, so peer.read_command()
        # has access to them
        {
            exp_key: getattr(work_defi_modu, exp_key)
            for exp_key in work_defi_modu.__all__
        }
    )

    is_coro_job = inspect.iscoroutinefunction(doOneJob_)

    # identify this connection as swarm worker to the headhunter, then it will
    # start sending ips to our data channel
    async def initiate_jobs():
        await peer.post_command(
            expr(
                """
StartWorking( {$ sedh.senv.swarmWorkerPid $}, {$ sedh.senv.swarmManagerPid $} )
"""
            )
        )

    async def process_jobs():
        async for ips in jobSink.run_producer(initiate_jobs()):
            logger.debug(f"Swarm worker received ips: {ips}")

            try:
                if is_coro_job:
                    result = await doOneJob_(**ips)
                else:
                    result = doOneJob_(**ips)

                logger.debug(
                    f"Swarm worker returning computed result: {result} for ips: {ips}"
                )
                await peer.p2c(DATA_CHAN, repr(result))
            except Exception as job_exc:
                logger.error(
                    f"Swarm worker failed computing result for ips: {ips}",
                    exc_info=True,
                )
                await peer.p2c(ERR_CHAN, repr(repr(job_exc)))
                break  # stop processing more jobs

    # apks are expected to be posted to a worker's data channel, one by one
    loop.create_task(process_jobs())

    eol = peer.eol
    try:

        while True:
            cmd_val = await peer.read_command()
            if cmd_val is EndOfStream:
                break
            if cmd_val is not None:
                logger.warn(
                    f"Unexpected peer command from HH via: {peer!r}\n  {cmd_val!r}"
                )

    except asyncio.CancelledError:
        pass

    except Exception as exc:
        logger.error("Error occurred with the HH.", exc_info=True)
        if not eol.done():
            eol.set_exception(exc)

    finally:
        logger.debug(f"Done with the HH via: {peer!r}")

        if not eol.done():
            eol.set_result(None)


asyncio.run(_run_())

