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
from sedh.demo.batch import *

# import the work definition module's effects into this work script is a
# workaround for effectful artifacts defined there to actually work
import sedh.demo.batch

effect_import(sedh.demo.batch)

dump_stacks_on_SIGQUIT()

GlobalConfig = {
    # sever IP,无需修改
    "swarmIface": "0.0.0.0",
    # 召集工作机的广播地址,无需修改
    "swarmAddr": "127.0.0.1",#"127.0.0.1",#
    # 广播端口，不同集群不一样的端口，确定之后也不应随意修改
    "swarmPort": 2022,  # 开发测试：3724  mhask1: 3725  mhask2: 3726 mhask3: 3727 mhask4: 3728  实盘：3722，3723
    # TODO 任务优先级
    "priority": 0,
    # 任务线程数
    "headcount": 1,
    # 集群配置参数，无需修改
    "cfwInterval": 3,
}

# hyper parameters
effect(
    dict(
        swarmIface=GlobalConfig["swarmIface"],
        swarmAddr=GlobalConfig["swarmAddr"],
        swarmPort=GlobalConfig["swarmPort"],
        priority=GlobalConfig["priority"],
        headcount=GlobalConfig["headcount"],
        cfwInterval=GlobalConfig["cfwInterval"],
    )
)
async def start_hh():
    hh = HeadHunter()
    hh.start_hunting()
    for i in range(5):
        await manage_this_work(headhunter=hh)
    await hh.finish_up()
    hh.stop()

asyncio.run(start_hh())
