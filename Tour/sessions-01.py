"""

this is an entry script to hook up some worker nodes from the swarm,
to accomplish the work defined in some module, with arbitrary overrides
possible from this module

it's a good idea to archive this file as part of the result data done by
the swarm, even version control them

don't put too much logic in this entry module, an entry module should just
like a comprehensive command line, don't program the command line, or you
are going to repeat yourself in duplicating works in such entry modules

"""

from typing import *
import asyncio

from edh import *
from sedh import *

# we use this work definition module
# pull in support artifacts exported from the work definition module
from sedh.demo.session import *


# hyper parameters
effect(
    dict(
        swarmIface="0.0.0.0",
        swarmAddr="127.0.0.1",
        swarmPort=3722,
        priority=0,
        headcount=3,
        cfwInterval=3,
    )
)


asyncio.run(run_sessions())
