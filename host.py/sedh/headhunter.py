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
        self, peer: Peer, pid: int,
    ):
        self.peer = peer
        self.pid = pid

        self.hc_employed = 0
        self.hc_working = 0


class Worker:
    __slots__ = (
        "peer",
        "pid",
        "manager",
    )

    def __init__(self, peer: Peer, pid: int, forager_pid: int):
        self.peer = peer
        self.pid = pid
        self.manager = forager_pid

        err_sink = peer.armed_channel(ERR_CHAN)
        # convert disconnection without result to error
        def worker_disconn(eol: asyncio.Future):
            exc = None
            try:
                exc = eol.exception()
            except (Exception, asyncio.CancelledError) as e:
                exc = e
            err_sink.publish(
                exc or EdhPeerError(peer.ident, "Swarm worker disconnected")
            )

        peer.eol.add_done_callback(worker_disconn)

    def __repr__(self):
        return f"<Worker pid={self.pid} via {self.peer.ident}>"


class HeadHunter:
    """
    Head hunter is the usual batch based worksource for swarms

    """

    def __init__(
        self, result_sink: EventSink, server_modu: str = "sedh.hh",
    ):
        loop = asyncio.get_running_loop()

        self.result_sink = result_sink
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
        self.finishing_up = False  # signal of no more jobs, i.e. to start finishing up

    def all_finished(self) -> bool:
        return self.finishing_up and self.pending_cntr == 0 and not self.pending_jobs

    def start_hunting(self):
        if self.hunting_task is None:
            self.hunting_task = asyncio.create_task(self._run())

    def __await__(self):
        self.start_hunting()
        return self.hunting_task

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
        peer = effect(netPeer)
        logger.debug(
            f"Worker process pid={worker_pid} managed by {forager_pid} via {peer.ident} start working."
        )
        worker = Worker(peer, worker_pid, forager_pid)
        self.idle_workers.append(worker)
        self.worker_available.set()

    async def _run(self):
        loop = asyncio.get_running_loop()

        swarm_iface = "0.0.0.0"
        try:
            swarm_iface = effect("swarmIface")
        except:
            pass

        def swarm_conn_init(modu: Dict):
            modu["OfferHeads"] = self.OfferHeads
            modu["StartWorking"] = self.StartWorking

        server = await EdhServer(
            self.server_modu,
            swarm_iface,  # local addr to bind
            0,  # local port to bind
            init=swarm_conn_init,
        )
        ws_sockets = server.server_sockets.result()
        if ws_sockets:
            ws_addrs = [sock.getsockname() for sock in ws_sockets]
            logger.info(f"HeaderHunter listening: {ws_addrs}")
        else:
            # or the network has failed, propagate the error
            await server.join()  # this usually throws
            # in case join() didn't throw, report this error
            raise RuntimeError("HeadHunter failed listening.")

        swarm_addr, swarm_port = "127.0.0.1", 3722
        try:
            swarm_addr = effect("swarmAddr")
        except:
            pass
        try:
            swarm_port = effect("swarmPort")
        except:
            pass

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

        cfw_interval = 3
        try:
            cfw_interval = effect("cfw_interval")
        except:
            pass

        while not self.all_finished():

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

            hc_demand = self.headcount - hc_employed
            if hc_demand > 0:
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
                [asyncio.sleep(cfw_interval), self.result_sink.one_more()],
                return_when=asyncio.FIRST_COMPLETED,
            )

    async def track_job(self, worker: Worker, ips: Dict):
        peer = worker.peer
        # arm a new private data/err channels to this job
        job_sink = peer.arm_channel(DATA_CHAN)
        # `asyncio.wait()` don't like a coroutine to appear in `aws` to it,
        # create a task (a Future object per se) for tracking purpose
        err_task = asyncio.create_task(peer.armed_channel(ERR_CHAN).one_more())

        async def wait_result():
            async for result in job_sink.run_producer(peer.p2c(DATA_CHAN, repr(ips))):
                self.result_sink.publish((ips, result))
                self.idle_workers.append(worker)
                self.worker_available.set()
                break  # one ips at a time

        wait_task = asyncio.create_task(wait_result())
        # submit ips and process result
        await asyncio.wait(
            [wait_task, err_task], return_when=asyncio.FIRST_COMPLETED,
        )
        # anyway it's not pending now
        self.pending_cntr -= 1
        # capture any possible exception that has failed the job
        jobExc = None
        if wait_task.done():
            try:
                wait_task.result()  # propagate any error ever occurred
                return  # job done successfully
            except Exception as jobExc:
                pass

        # this job didn't make it
        peer.stop()  # disconnect this worker anyway if no-result

        # let off `wait_result()`, or it's leaked
        job_sink.publish(EndOfStream)

        try:
            if err_task.done():
                jobExc = err_task.result()
            else:
                err_task.cancel()  # prevent leakage
        except Exception as jobExc:
            pass

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
                        self.hunting_task,
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
        self.finishing_up = True
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

