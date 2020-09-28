from typing import *
import asyncio

from hastalk import *

logger = get_logger(__package__)


# a Peer object should have been implanted automatically, the following
# line should have no practical effect but to hint an IDE for code
# completion etc.
if peer is None:
    peer = Peer()

logger.debug(f"Comm with forager/worker via: {peer!r}")


peer.ensure_channel(CONIN)


async def conout_intake(sink):
    async for con_out in sink.stream():
        print(con_out)


asyncio.create_task(conout_intake(peer.ensure_channel(CONOUT)))


async def conmsg_intake(sink):
    async for con_msg in sink.stream():
        logger.info(con_msg)


asyncio.create_task(conmsg_intake(peer.ensure_channel(CONMSG)))


async def peer_err_intake(sink):
    async for peer_err in sink.stream():
        logger.error(peer_err)
        if not peer.eol.done():
            peer.eol.set_exception(RuntimeError(peer_err))


asyncio.create_task(peer_err_intake(peer.ensure_channel(ERR_CHAN)))


async def _run_():
    import hastalk.nedh.effects

    effect_import(hastalk.nedh.effects)

    effect(
        {netPeer: peer, dataSink: peer.ensure_channel(DATA_CHAN),}
    )

    eol = peer.eol
    try:

        while True:
            cmd_val = await peer.read_command()
            if cmd_val is EndOfStream:
                break
            if cmd_val is not None:
                logger.warn(
                    f"Unexpected peer command from forager/worker via: {peer!r}\n  {cmd_val!r}"
                )

    except asyncio.CancelledError:
        pass

    except Exception as exc:
        logger.error("Error occurred with the forager/worker.", exc_info=True)
        if not eol.done():
            eol.set_exception(exc)

    finally:
        logger.debug(f"Done with the forager/worker via: {peer!r}")

        if not eol.done():
            eol.set_result(None)

        peer.armed_channel(CONIN).publish(EndOfStream)
        peer.armed_channel(CONOUT).publish(EndOfStream)
        peer.armed_channel(CONMSG).publish(EndOfStream)
        peer.armed_channel(DATA_CHAN).publish(EndOfStream)


# schedule the command landing loop to run in a dedicated task (thread)
asyncio.create_task(_run_())

