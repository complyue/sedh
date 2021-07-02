#!/usr/bin/env python3

import subprocess, re, socket, json, sys
import datetime, psutil


ts = datetime.datetime.now().astimezone().replace(microsecond=0).isoformat()
vmem = psutil.virtual_memory()
swap = psutil.swap_memory()
cpu_freq = psutil.cpu_freq()
cpu_load = psutil.cpu_percent(interval=3, percpu=True)
nps = sum(1 for _ in psutil.process_iter())


addr, port = "255.255.255.255", 6768
try:
    if len(sys.argv) == 3:
        port = int(sys.argv[1])
        addr = sys.argv[2]
    elif len(sys.argv) == 2:
        port = int(sys.argv[1])
except Exception as ex:
    print("Args error: " + ex, file=sys.stderr)
    print("Usage: rptup.py [ port [addr] ]", file=sys.stderr)
    sys.exit(1)

with socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP) as sock:
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)

    mac = None
    for line in (
        subprocess.check_output(["/usr/bin/env", "ip", "addr", "show"])
        .decode("utf-8")
        .splitlines()
    ):
        mac_match = re.search("\s+link/ether (\S+)", line)
        if mac_match:
            mac = mac_match.group(1)
            continue

        if mac is None:
            continue

        ip_match = re.search("\s+inet (\S+)/", line)
        if ip_match:
            ip = ip_match.group(1)

            announcement = json.dumps(
                {
                    "mac": mac,
                    "ip": ip,
                    "heartbeat": {
                        "timestamp": ts,
                        "vmem": {f: getattr(vmem, f) for f in vmem._fields},
                        "swap": {f: getattr(swap, f) for f in swap._fields},
                        "cpufreq": cpu_freq,
                        "cpuload": cpu_load,
                        "nps": nps,
                    },
                }
            )

            print(f"Reporting to [{addr}:{port}]", file=sys.stderr)
            # print(announcement, file=sys.stderr)
            sock.sendto(announcement.encode("utf-8"), (addr, port))
