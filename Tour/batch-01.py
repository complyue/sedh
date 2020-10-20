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

# work definition scripts are allowed to change the inferred configuration at
# `sedh.senv`, import the module as a reference, to get/set effective
# artifacts living there
import sedh.senv

# we use this work definition module
# pull in support artifacts exported from the work definition module
from sedh.demo.batch import *

# import the work definition module's effects into this work script is a
# workaround for effectful artifacts defined there to actually work
import sedh.demo.batch

effect_import(sedh.demo.batch)

logger = sedh.log.get_logger("sedh.demo.tour")


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


# ad-hoc parameter overrides
def m_range():
    for m in range(7, 9):
        yield m


asyncio.run(
    manage_this_work(
        # what's specified here will override artifacts those samely named in
        # the reused work definition module's scope, they serve as lexical
        # default values to `manage_this_work()` defined there
        m_range=m_range,
    )
)

