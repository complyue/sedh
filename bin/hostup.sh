#!/usr/bin/env bash

cc="192.168.11.253:6780"

nic=$(ls -1d /sys/class/net/* | grep -v docker | grep -v lo | head -1)
mac=$(cat $nic/address | grep -v '00:00:00:00:00:00')

if ! hn=$(curl -f -s -d 'hostname' -X POST http://$cc/cnode/v1/eval/$mac) ; then
  echo "No hostname configured, try it yourself: "
  echo "curl -s -d 'hostname' -X POST http://$cc/cnode/v1/eval/$mac"
  exit 1
fi

echo "Setting hostname to [$hn] ..."

hostname $hn
