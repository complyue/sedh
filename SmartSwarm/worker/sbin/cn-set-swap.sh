#!/bin/bash

# exit when any command fails
set -e

# generate a new uuid to form unique swap file name
SWAPID=$(hostname)~$(uuid)
# swap be half of physical RAM size
SWAPSZ=$(awk '/MemTotal/ {print $2 / 2 }' /proc/meminfo)

# create files not readable by other users
umask 077

# create swap file
truncate -s ${SWAPSZ}000 /cnswap/${SWAPID}

# setup loop-back device
losetup /dev/loop0 /cnswap/${SWAPID}

# format as swap
mkswap /dev/loop0 >/dev/null

# turn on swap on it
swapon /dev/loop0

# record the file name
echo "/cnswap/${SWAPID}" > /run/cn-swapfile

# make the file auto deleted once closed, avoid garbage file
# left in case compute node crash etc.
unlink /cnswap/${SWAPID}
