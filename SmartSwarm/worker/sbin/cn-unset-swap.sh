#!/bin/bash

swapoff /dev/loop0
losetup -d /dev/loop0
echo "*off*" >> /run/cn-swapfile
