"""
"""

__all__ = ()

import os, sys


# default to launch a Python work module
jobExectuable = [
    sys.executable,
    "-m",
    "hastalk.gwd",
]

# default to use current working directory
jobWorkDir = os.getcwd()

# default to the entry module
jobWorkSpec = sys.modules["__main__"].__package__

