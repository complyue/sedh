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
jobWorkSpec = sys.argv[0]


# os pid of forager process
swarmManagerPid = os.getppid()

# os pid of worker process
swarmWorkerPid = os.getpid()

# os file descripter number for the socket connected to hh from worker
wscFd = 15

if len(sys.argv) == 3:
    jobWorkSpec, wscFds = sys.argv[1:]
    wscFd = int(wscFds)
