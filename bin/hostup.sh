#!/usr/bin/env bash

cc="192.168.13.20:6780"

mac=$(cat /sys/class/net/*/address | grep -v '00:00:00:00:00:00')

if ! hn=$(curl -f -s -d 'hostname' -X POST http://$cc/cnode/v1/eval/$mac) ; then
  echo "No hostname configured, try it yourself: "
  echo "curl -s -d 'hostname' -X POST http://$cc/cnode/v1/eval/$mac"
  exit 1
fi

echo "Setting hostname to [$hn] ..."

hostname $hn
