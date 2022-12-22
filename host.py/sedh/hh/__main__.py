from typing import *
import asyncio

from edh import *
from nedh import *
from sedh import *

import sedh

logger = sedh.log.get_logger(__package__)


# a Peer object should have been implanted automatically, the following
# line should have no practical effect but to hint an IDE for code
# completion etc.
if peer is None:
    peer = Peer()

logger.debug(f"Comm with forager/worker via: {peer!r}")


async def _run_():
    import nedh.effects

    effect_import(nedh.effects)

    effect(
        {
            netPeer: peer,
        }
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


# schedule the command landing loop to run in a dedicated task (thread)
asyncio.create_task(_run_())
