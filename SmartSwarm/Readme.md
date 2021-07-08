# Smart Swarm

SmartOS<sup>TM</sup> can serve well as the storage server, for shared filesystems to be remotely mounted via `NFS` by many worker machines. It can also assume the boot server, to allow those worker machines go entirely diskless.

## Swarm Controller / Storage Server

One server with lots of RAM, disks, and one or more fast NICs

See [Controller Setup](./ControllerSetup.md)

## Swarm Workers

Lots of diskless servers, each with CPUs, RAM, and one or more PXE bootable NICs (onboard cards usually do)

See [Worker Setup](./WorkerSetup.md)
