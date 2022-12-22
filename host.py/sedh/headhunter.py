__all__ = ["HeadHunter"]

from typing import *
import asyncio
import inspect

from edh import *
from nedh import *

from . import log
from .symbols import *

# work definition scripts are allowed to change the inferred
# configuration at `sedh.senv`, import it as a namespace to
# always use up-to-date artifacts living there
import sedh.senv as senv

logger = log.get_logger(__name__)


class Forager:
    __slots__ = (
        "peer",
        "pid",
        "hc_employed",
        "hc_working",
    )

    def __init__(
        self,
        peer: Peer,
        pid: int,
    ):
        self.peer = peer
        self.pid = pid

        self.hc_employed = 0
        self.hc_working = 0


class Worker:
    __slots__ = ("peer", "pid", "manager", "_jobs_quota_left", "err_task")

    def __init__(self, peer: Peer, pid: int, forager_pid: int):
        self.peer = peer
        self.pid = pid
        self.manager = forager_pid
        self._jobs_quota_left = 10  # TODO out as a param

        peer.ensure_channel(DATA_CHAN)
        err_chan = peer.ensure_channel(ERR_CHAN)
        self.err_task = asyncio.create_task(err_chan.get())

        # convert disconnection without result to error
        def worker_disconn(eol: asyncio.Future):
            exc = None
            try:
                exc = eol.exception()
            except (Exception, asyncio.CancelledError) as e:
                exc = e
            asyncio.create_task(
                err_chan.put(
                    exc or EdhPeerError(peer.ident, "Swarm worker disconnected")
                )
            )

        peer.eol.add_done_callback(worker_disconn)

    def check_jobs_quota(self):
        self._jobs_quota_left -= 1
        if self._jobs_quota_left <= 0:
            self.peer.stop()
            return False
        return True

    def __repr__(self):
        return f"<Worker pid={self.pid} via {self.peer.ident}>"


class HeadHunter:
    """
    Head hunter is the usual batch based worksource for swarms

    """

    def __init__(
        self,
        result_ch: Optional[BChan] = None,
        server_modu: str = "sedh.hh",
    ):
        self.result_ch = result_ch or BChan()
        self.server_modu = server_modu

        # fetch effective configurations, cache as instance attribute
        self.priority = effect("priority")
        self.headcount = effect("headcount")
        self.workModu = effect(workDefinition)
        self.shouldRetryJob = effect(shouldRetryJob)

        self.hc_employed = 0
        self.foragers = {}
        self.idle_workers = []  # FILO queue
        self.worker_available = asyncio.Event()

        self.hunting_task = None

        self.pending_jobs = []  # FILO queue
        self.pending_cntr = 0  # increased/decreased on job submit/result/err
        # signaled once no more jobs, i.e. to start finishing up
        self.finishing_up = asyncio.Event()

        self.net_server = None

    def stop(self):
        server = self.net_server
        if server is not None:
            server.stop()
        while self.idle_workers:
            worker = self.idle_workers.pop()
            worker.peer.stop()
        while self.foragers:
            peer, _forager = self.foragers.popitem()
            peer.stop()

    def all_finished(self) -> bool:
        return (
            self.finishing_up.is_set()
            and self.pending_cntr == 0
            and not self.pending_jobs
        )

    def start_hunting(self):
        if self.hunting_task is None:
            self.hunting_task = asyncio.create_task(self._run())
            self.hunting_task.add_done_callback(lambda _: self.result_ch.close())
        return self.hunting_task

    def __await__(self):
        self.start_hunting()
        yield from self.hunting_task

    async def get_idle_worker(self):
        while True:
            if self.idle_workers:
                worker = self.idle_workers.pop()
                return worker

            logger.debug("Wait for idle workers.")
            self.worker_available.clear()
            await asyncio.wait(
                [
                    asyncio.shield(self.hunting_task),
                    asyncio.create_task(self.worker_available.wait()),
                ],
                return_when=asyncio.FIRST_COMPLETED,
            )
            if self.hunting_task.done():
                # propagate any error ever occurred
                await self.hunting_task
                raise asyncio.CancelledError("HH done, no more worker to offer.")
            logger.debug("Got idle workers.")

    async def release_idle_worker(self, worker: Worker):
        self.idle_workers.append(worker)
        self.worker_available.set()

    # a swarm node connection identifying itself as a forager
    async def OfferHeads(self, forager_pid: int, max_hc: int):
        peer = effect(netPeer)

        hc_demand = self.headcount - self.hc_employed
        hc2employ = min(max_hc, hc_demand)

        if hc2employ < 1:  # no demand, disconnect
            # todo send zero employment ?
            peer.stop()
            return

        # employ heads from this forager
        forager = self.foragers.get(peer, None)
        if forager is None:
            forager = Forager(peer, forager_pid)
            self.foragers[peer] = forager
        forager.hc_employed += hc2employ
        logger.debug(f"HH is hiring {forager.hc_employed} heads from {peer.ident} now.")
        await peer.p2c(DATA_CHAN, repr(forager.hc_employed))

    # a swarm node connection identifying itself as a worker
    async def StartWorking(self, worker_pid: int, forager_pid: int):
        peer: Peer = effect(netPeer)
        logger.debug(
            f"Worker process pid={worker_pid} managed by {forager_pid} via {peer.ident} start working."
        )
        worker = Worker(peer, worker_pid, forager_pid)
        self.idle_workers.append(worker)
        self.worker_available.set()

    async def _run(self):
        loop = asyncio.get_running_loop()

        swarm_iface = effect("swarmIface", "0.0.0.0")

        def swarm_conn_init(modu: Dict):
            modu["OfferHeads"] = self.OfferHeads
            modu["StartWorking"] = self.StartWorking

        server = await EdhServer(
            self.server_modu,
            swarm_iface,  # local addr to bind
            0,  # local port to bind
            init=swarm_conn_init,
        )
        self.net_server = server
        ws_sockets = server.server_sockets.result()
        if ws_sockets:
            ws_addrs = [sock.getsockname() for sock in ws_sockets]
            logger.info(f"HeaderHunter listening: {ws_addrs}")
        else:
            # or the network has failed, propagate the error
            await server.join()  # this usually throws
            # in case join() didn't throw, report this error
            raise RuntimeError("HeadHunter failed listening.")

        swarm_addr = effect("swarmAddr", "127.0.0.1")
        swarm_port = effect("swarmPort", 3722)

        # todo release cfw on cleanup.
        # but not urgent as long as HH runs in one-shot manner, the process is
        # terminating anyway as soon as an HH is done.
        cfw_trans, _protocol = await loop.create_datagram_endpoint(
            lambda: WorkAnnouncingProtocol(),
            # announce call-for-workers from the tcp listening addr as worksource
            family=ws_sockets[0].family,
            local_addr=ws_addrs[0],
            allow_broadcast=True,
        )

        cfw_interval = effect("cfw_interval", 3)

        while not self.finishing_up.is_set():

            # calibrate self.hc_employed, wrt forager disconnection etc.
            hc_employed = 0
            for peer, forager in self.foragers.copy().items():
                if peer.eol.done():
                    del self.foragers[peer]  # forget about it
                else:
                    hc_employed += forager.hc_employed
            if hc_employed != self.hc_employed:
                logger.debug(f"HH has {hc_employed} heads employed now.")
                self.hc_employed = hc_employed

            logger.info(f"headcount-------------{self.headcount}  {hc_employed}  {len(self.idle_workers)}")
            hc_demand = self.headcount - hc_employed
            if hc_demand < 0:
                pass  # TODO reduce employed headcounts gradually
            elif hc_demand > 0:
                logger.debug(
                    f"HH calling {hc_demand} workers from {swarm_addr!s}:{swarm_port!s} via {ws_addrs[0]}"
                )
                pkt = f"""
WorkToDo(
    {hc_demand!r},
    {senv.jobExecutable!r},
    {senv.jobWorkDir!r},
    {self.workModu!r},
    {self.priority!r},
)
""".encode()
                cfw_trans.sendto(pkt, (swarm_addr, swarm_port))

            await asyncio.wait(
                [asyncio.sleep(cfw_interval), self.finishing_up.wait()],
                return_when=asyncio.FIRST_COMPLETED,
            )

        while not self.all_finished():
            await asyncio.sleep(0.2)

    async def track_job(self, worker: Worker, ips: Dict):
        peer = worker.peer
        # arm a new private data/err channels to this job
        # job_sink = peer.arm_channel(DATA_CHAN)
        # # `asyncio.wait()` don't like a coroutine to appear in `aws` to it,
        # # create a task (a Future object per se) for tracking purpose
        # err_task = asyncio.create_task(peer.armed_channel(ERR_CHAN).one_more())
        # # worker_ip = eval(worker.peer.ident)[0]
        # # logger.info(F"IP[{worker_ip}]---ips={ips}")
        # async def wait_result():
        #     got_result = False
        #     async for result in job_sink.run_producer(peer.p2c(DATA_CHAN, repr(ips))):
        #         got_result = True
        #         if worker.check_jobs_quota():
        #             self.idle_workers.append(worker)
        #             self.worker_available.set()
        #         break  # one ips at a time
        #     if not got_result:
        #         raise EOFError("未能收回当前参数的结果")

        job_chan = peer.armed_channel(DATA_CHAN)

        # submit ips and process result

        timeout_ = ips["timeout"] if "timeout" in ips else None
        # done, pending = await asyncio.wait(
        #     [wait_task, err_task],
        #     timeout=timeout_,

        wait_task = asyncio.create_task(job_chan.get())
        await peer.p2c(DATA_CHAN, repr(ips))
        await asyncio.wait(
            [wait_task, worker.err_task],
            timeout=timeout_,
            return_when=asyncio.FIRST_COMPLETED,
        )
        # anyway it's not pending now
        self.pending_cntr -= 1
        # capture any possible exception that has failed the job
        jobExc = None
        if wait_task.done():
            try:
            #     wait_task.result()  # propagate any error ever occurred
            #     return  # job done successfully
            # except Exception as exc:
            #     jobExc = exc
                result = wait_task.result()  # propagate any error ever occurred
                # job done successfully
                if worker.check_jobs_quota():
                    self.idle_workers.append(worker)
                    self.worker_available.set()
                await self.result_ch.put((ips, result))
                return
            except Exception as jobExc:
                pass

        # this job didn't make it
        peer.stop()  # disconnect this worker anyway if no-result

        # let off `wait_result()`, or it's leaked
        # job_sink.publish(EndOfStream)

        # 将以下三个参数放入ips中
        # timeout: 超时时间，单位:秒    
        # retry_cnt: 重试次数
        # retry_maxN: 最大重试次数限制
        
        if "retry_cnt" in ips and "retry_maxN" in ips:
            if ips["retry_cnt"] >= ips["retry_maxN"]:
                logger.error(f"当前参数重跑次数已满，请检查参数正确性!  ips={ips}")
                return
            ips["retry_cnt"] += 1
            if not worker.err_task.done():
                logger.warning(f"当前参数已超时，开始重跑，当前尝试次数{ips['retry_cnt']}  ips={ips}")
        try:
            if worker.err_task.done():
                jobExc = worker.err_task.result()
        except Exception as exc:
            jobExc = exc
        # try:
        #     if worker.err_task.done():
        #         jobExc = worker.err_task.result()
        # except Exception as jobExc:
        #     pass

        try:
            if self.shouldRetryJob is False:
                pass
            elif self.shouldRetryJob is True:
                self.pending_jobs.append(ips)
            else:
                maybe_coro = self.shouldRetryJob(jobExc, ips)
                if inspect.isawaitable(maybe_coro):
                    new_ips = await maybe_coro
                else:
                    new_ips = maybe_coro
                if new_ips is None:
                    logger.info("ips is None!抛弃该组任务！！")
                    return
                logger.info(f"retry job for ips={ips}")
                self.pending_jobs.append(new_ips)
        except Exception:
            logger.error(f"Failed to retry job for ips={ips!r}", exc_info=True)

    async def submit_jobs(self):
        while self.pending_jobs:
            ips = self.pending_jobs.pop()
            self.pending_cntr += 1

            while True:
                if self.idle_workers:
                    worker = self.idle_workers.pop()
                    logger.debug(f"Job assigned to {worker} - {ips}")

                    worker_ip = eval(worker.peer.ident)[0]
                    ips["worker_ip"] = worker_ip
                    logger.debug(f"worker IP:  {worker_ip}")

                    track_task = asyncio.create_task(self.track_job(worker, ips))

                    def track_done(fut: asyncio.Task):
                        if fut.cancelled():
                            return  # todo sth to do in this case?
                        trackExc = fut.exception()
                        if trackExc is not None:
                            logger.error(
                                f"Failed tracking job for ips={ips!r} - {trackExc!s}\n{track_task.get_stack()!s}"
                            )
                            worker.peer.stop()

                    track_task.add_done_callback(track_done)
                    break

                logger.debug("Wait for idle workers.")
                self.worker_available.clear()
                await asyncio.wait(
                    [
                        asyncio.shield(self.hunting_task),
                        asyncio.create_task(self.worker_available.wait()),
                    ],
                    return_when=asyncio.FIRST_COMPLETED,
                )
                if self.hunting_task.done():
                    # propagate any error ever occurred
                    await self.hunting_task
                    return
                logger.debug("Got idle workers.")

    async def dispatch_job(self, ips: Dict):
        self.pending_jobs.append(ips)
        await self.submit_jobs()

    async def finish_up(self):
        # self.finishing_up = True
        # while True:
        #     if self.all_finished():
        #         return
        #     if self.pending_jobs:
        #         await self.submit_jobs()
        #     if self.hunting_task.done():
        #         # propagate any error ever occurred
        #         await self.hunting_task
        #         return
        #     await asyncio.sleep(2)
        self.finishing_up.set()
        if self.all_finished():
            return
        await self.hunting_task


class WorkAnnouncingProtocol(asyncio.DatagramProtocol):
    """
    this is a send-only protocol, call-for-workers announcements will be
    sent out with the UDP transport by broadcast or unicast.

    """

    __slots__ = ()

    def connection_made(self, transport):
        pass  # nothing to do

    def datagram_received(self, data, addr):
        pass  # ignore any UDP reply

    def error_received(self, exc):
        logger.warning(f"HH call-for-workers got UDP error: {exc!s}")

    def connection_lost(self, exc):
        pass
