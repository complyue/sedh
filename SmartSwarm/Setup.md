# Smart Swarm

SmartOS<sup>TM</sup> controlled/backed cluster of Linux worker machines

- [Configuration](#configuration)
  - [Hardware](#hardware)
    - [Swarm Controller](#swarm-controller)
    - [Swarm Workers](#swarm-workers)
  - [Software](#software)
    - [Operating System](#operating-system)
    - [Additional Packages](#additional-packages)
- [Setup Procedure](#setup-procedure)

## Configuration

### Hardware

#### Swarm Controller

One server with lots of RAM, disks, and one or more fast NICs

#### Swarm Workers

Many servers each with CPUs, RAM, and at least one net bootable (PXE ROM) NIC

### Software

#### Operating System

- Swarm Controller

  - SmartOS (Illumos) - joyent_20210617T001230Z

- Swarm Workers

  - Ubuntu - 20.04.2 LTS

#### Additional Packages

- Swarm Controller

  - GCC - 9.3.0
  - Go - 1.15.12
  - GHC - 8.8.4
  - Cabal-install - 3.0.0.0
  - Đ - 0.3
  - SĐ -

- Swarm Workers

  - GCC - 9.3.0
  - GHC - 8.8.4
  - Cabal-install -
  - Stack -
  - ĐPM - 0.3.1
  - Đ - 0.3
  - SĐ -

## Setup Procedure
