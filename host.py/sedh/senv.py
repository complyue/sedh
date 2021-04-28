"""
"""

__all__ = ()

import os, sys


# default to launch a Python work module
jobExecutable = [
    "epm",
    "x",
    "python",
    "-m",
    "sedh.worker",
]

# default to use current working directory
jobWorkDir = os.getcwd()


# - in HH process:
#  the .py file on command line of python, the work scheduler script
# - in worker process:
#  name of the work definition module
def jobWorkSpec():
    if len(sys.argv) == 3:
        jobWorkSpec, _wscFds = sys.argv[1:]
        return jobWorkSpec
    raise RuntimeError("Not a recognized Sedh worker cmdl: " + sys.argv)


# os pid of forager process
swarmManagerPid = os.getppid()

# os pid of worker process
swarmWorkerPid = os.getpid()

# os file descripter number for the socket connected to hh from worker
def wscFd():
    if len(sys.argv) == 3:
        _jobWorkSpec, wscFds = sys.argv[1:]
        return int(wscFds)
    raise RuntimeError("Not a recognized Sedh worker cmdl: " + sys.argv)
