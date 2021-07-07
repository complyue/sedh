# Setup a Swarm Controller running SmartOS<sup>TM</sup>

- [Advisories](#advisories)
  - [Hardware Planning](#hardware-planning)
  - [Plenty RAM & Decent CPU](#plenty-ram--decent-cpu)
  - [RAID be just Mirror](#raid-be-just-mirror)
  - [USB Stick Booting](#usb-stick-booting)
- [Setup Procedure](#setup-procedure)
  - [OS Install](#os-install)
  - [Advised Performance Tweaks](#advised-performance-tweaks)
  - [Prepare Filesystems](#prepare-filesystems)
  - [Setup a Swarm Controller Zone](#setup-a-swarm-controller-zone)

## Advisories

### Hardware Planning

For SmartOS or other Illumos derivations, there are usually no separate driver software for hardwares from their respective vendors, you best bet is to consult the HCL at:

https://illumos.org/hcl

### Plenty RAM & Decent CPU

- The more RAM the better, 8GB may be a bare minimum
- CPU requirements are quite moderate, decent x64 ones should all do

### RAID be just Mirror

- Install as many of HDDs (magnetic spinning disks) as possible, with one or more of them designated as hot spare
- Go three way mirror, or else, normal mirror, forget about other RAID options
- Optimize write performance with SSDs in case you need, though mind to leave more hot spares there

### USB Stick Booting

SmartOS<sup>TM</sup> updates regularly, you don't need to follow it tightly, but by booting with a USB stick, you can simply switch in a new stick with updated os image, and reboot, then all will continue working with security/bug fixes as well as new features.

## Setup Procedure

### OS Install

https://wiki.smartos.org/install

### Advised Performance Tweaks

> This may not fit your particular needs, do it at your own risk

```console
zfs set sync=disabled dedup=on compress=on atime=off zones
```

### Prepare Filesystems

- For swarm controller

> It'll run as a native zone, a separate ZFS filesystem will be used to store software & configurations for the `Swarm Control Center` service

```console
zfs create -o mountpoint=/swarmcc -o quota=20G -o reservation=6G zones/swarmcc
```

- For swarm workers (computing nodes)

> The worker machines will run diskless, i.e. net booted into Linux with `NFS` mounted, shared, readonly root filesystem, and other writable filesystems shared

> Several ZFS filesystems are to be created for the run of worker machines, including:

- The usually readonly root filesystem
- The shared, usually writable `/var` filesystem
- The shared writable filesystem to persist swap files per worker machine
- The shared writable workspace to store artifacts as payload of swarm works

> Change the network address and quota/reservation per your needs

```console
zfs create -o mountpoint=/cnroot -o sharenfs=root=@10.0.0.0/8 -o quota=20G -o reservation=10G zones/cnroot
zfs create -o mountpoint=/cnvar -o sharenfs=root=@10.0.0.0/8 -o quota=20G -o reservation=10G zones/cnvar
zfs create -o mountpoint=/cnswap -o sharenfs=root=@10.0.0.0/8 -o quota=80G -o reservation=60G zones/cnswap
zfs create -o mountpoint=/cnwkspc -o sharenfs=root=@10.0.0.0/8 -o quota=80G -o reservation=60G zones/cnwkspc

```

### Setup a Swarm Controller Zone

- Import the latest `minimal-64` zone image

```console
# imgadm available | grep minimal-64
5a4ba06a-c1bb-11e4-af0b-4be0ce4ce04c  minimal-64-lts                  14.4.0        smartos  zone-dataset  2015-03-03
75d1b5d8-e509-11e4-a51f-2fd538c62d87  minimal-64-lts                  14.4.1        smartos  zone-dataset  2015-04-17
b01dcb8e-ea5c-11e4-a7c8-cb7d4876960e  minimal-64                      15.1.0        smartos  zone-dataset  2015-04-24
 ...
e1a0b1ec-c690-11ea-8e16-d3e64a9e7fe3  minimal-64                      20.2.0        smartos  zone-dataset  2020-07-15
1c0a91c8-03f8-11eb-88cc-07946e332af4  minimal-64                      20.3.0        smartos  zone-dataset  2020-10-01
9ad5a702-11e7-11eb-b237-3be202719561  minimal-64-trunk                20201019      smartos  zone-dataset  2020-10-19
800db35c-5408-11eb-9792-872f658e7911  minimal-64-lts                  20.4.0        smartos  zone-dataset  2021-01-11
# imgadm import 800db35c-5408-11eb-9792-872f658e7911
 ...
#
```

- Create the `swarmcc` zone

> Replace the uuid in `"image_uuid": "800db35c-5408-11eb-9792-872f658e7911"` with the one you actually imported

> Use alternate dns resolvers as you prefer

> Use static ip instead of `"ip": "dhcp",` as you prefer, e.g.

```json
    "ip": "10.88.88.52",
    "netmask": "255.255.255.0",
    "gateway": "10.88.88.2"
```

> Note down the uuid of the zone vm you actually created, you'll replace subsequent occurrences of `06536e48-ae04-44a8-8996-f5d7a39612a4` with that, for demonstrated commands in rest of this document

> Note `GHC` is pretty RAM hungry in compiling sufficiently sophsticated projects, so `max_physical_memory` can't be too low, the typical `4096` prevalently elsewhere is not sufficient here

```console
# cat swarmcc.json
{
  "alias": "swarmcc",
  "hostname": "swarmcc",
  "brand": "joyent-minimal",
  "image_uuid": "800db35c-5408-11eb-9792-872f658e7911",
  "autoboot": true,
  "max_physical_memory": 8000,
  "quota": 20,
  "resolvers": [
    "223.5.5.5",
    "119.29.29.29"
  ],
  "nics": [
    {
      "nic_tag": "admin",
      "dhcp_server": true,
      "allow_dhcp_spoofing": true,
      "allow_ip_spoofing": true,
      "allow_mac_spoofing": true,
      "allow_restricted_traffic": true,
      "allow_unfiltered_promisc": true,
      "ip": "dhcp",
      "primary": 1
    }
  ],
  "filesystems": [
    {
      "source": "/swarmcc",
      "target": "/swarmcc",
      "type": "lofs"
    }
  ]
}

# vmadm create -f swarmcc.json
Successfully created VM 06536e48-ae04-44a8-8996-f5d7a39612a4
#
```

- Login to the zone, set locale (especially the UTF-8 encoding) globally, re-login

```console
# zlogin 06536e48-ae04-44a8-8996-f5d7a39612a4
[Connected to zone '06536e48-ae04-44a8-8996-f5d7a39612a4' pts/4]
   __        .                   .
 _|  |_      | .-. .  . .-. :--. |-
|_    _|     ;|   ||  |(.-' |  | |
  |__|   `--'  `-' `;-| `-' '  ' `-'
                   /  ; Instance (minimal-64-lts 20.4.0)
                   `-'  https://docs.joyent.com/images/smartos/minimal

[root@swarmcc ~]# echo 'export LANG=C.UTF-8' >> /etc/profile
[root@swarmcc ~]# logout

[Connection to zone '06536e48-ae04-44a8-8996-f5d7a39612a4' pts/4 closed]
# zlogin 06536e48-ae04-44a8-8996-f5d7a39612a4
[Connected to zone '06536e48-ae04-44a8-8996-f5d7a39612a4' pts/4]
Last login: Wed Jul  7 14:17:11 on pts/4
   __        .                   .
 _|  |_      | .-. .  . .-. :--. |-
|_    _|     ;|   ||  |(.-' |  | |
  |__|   `--'  `-' `;-| `-' '  ' `-'
                   /  ; Instance (minimal-64-lts 20.4.0)
                   `-'  https://docs.joyent.com/images/smartos/minimal

[root@swarmcc ~]# locale
LANG=C.UTF-8
LC_CTYPE="C.UTF-8"
LC_NUMERIC="C.UTF-8"
LC_TIME="C.UTF-8"
LC_COLLATE="C.UTF-8"
LC_MONETARY="C.UTF-8"
LC_MESSAGES="C.UTF-8"
LC_ALL=
[root@swarmcc ~]#
```

- Refresh `pkgin` source

```console
[root@swarmcc ~]# pkgin update
reading local summary...
processing local summary...
processing remote summary (https://pkgsrc.joyent.com/packages/SmartOS/2020Q4/x86_64/All)...
pkg_summary.xz                                   100% 2416KB  51.4KB/s   00:47
[root@swarmcc ~]#
```

- Install Git, GCC, GHC, Cabal-install, Go

```console
[root@swarmcc ~]# pkgin in git gcc9 ghc cabal-install go
calculating dependencies...done.
 ...
proceed ? [Y/n]
 ...
pkg_install warnings: 0, errors: 0
reading local summary...
processing local summary...
marking git-2.29.2 as non auto-removable
marking gcc9-9.3.0 as non auto-removable
marking ghc-8.8.4nb1 as non auto-removable
marking cabal-install-3.0.0.0nb2 as non auto-removable
marking go-1.15.12 as non auto-removable
[root@swarmcc ~]#
```

- Update Cabal

```console
[root@swarmcc ~]# cabal update
Downloading the latest package list from mirrors.tuna.tsinghua.edu.cn
[root@swarmcc ~]#
```

- Install ĐPM, SĐ

```console
[root@swarmcc ~]# curl -o /opt/local/bin/epm -L https://github.com/e-wrks/epm/raw/latest/epm
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   121  100   121    0     0    177      0 --:--:-- --:--:-- --:--:--   177
100 36400  100 36400    0     0  26759      0  0:00:01  0:00:01 --:--:-- 26759
[root@swarmcc ~]# chmod a+x /opt/local/bin/epm
[root@swarmcc ~]# cd /swarmcc
[root@swarmcc /swarmcc]# epm init
[root@swarmcc /swarmcc]# epm i edh nedh sedh
 ℹ️  Installing edh to edh-universe/e-wrks/edh ...
Cloning into 'edh-universe/e-wrks/edh'...
remote: Counting objects: 11275, done.
remote: Compressing objects: 100% (3457/3457), done.
remote: Total 11275 (delta 6839), reused 8822 (delta 5327)
Receiving objects: 100% (11275/11275), 2.21 MiB | 1.50 MiB/s, done.
Resolving deltas: 100% (6839/6839), done.
 ℹ️  Installed edh .
 ℹ️  Installing nedh to edh-universe/e-wrks/nedh ...
Cloning into 'edh-universe/e-wrks/nedh'...
remote: Counting objects: 2712, done.
remote: Compressing objects: 100% (1299/1299), done.
Receiving objectsremote: Total 2712 (delta 1380), reused 1435 (delta 697)
Receiving objects: 100% (2712/2712), 475.60 KiB | 667.00 KiB/s, done.
Resolving deltas: 100% (1380/1380), done.
 ℹ️  Installed nedh .
 ℹ️  Installing sedh to edh-universe/e-wrks/sedh ...
Cloning into 'edh-universe/e-wrks/sedh'...
remote: Counting objects: 1711, done.
remote: Compressing objects: 100% (1068/1068), done.
remote: Total 1711 (delta 804), reused 659 (delta 286)
Receiving objects: 100% (1711/1711), 227.52 KiB | 485.00 KiB/s, done.
Resolving deltas: 100% (804/804), done.
 ℹ️  Installed sedh .
[root@swarmcc /swarmcc]# cd edh-universe/
```

- Install patched `network` to workaround the compilation errors

> You don't do this once https://github.com/haskell/network/issues/506 get solved

```console
[root@swarmcc /swarmcc/edh-universe]# epm i -b fix506 https://github.com/complyue/network.git
 ℹ️   >> Managing packages at EPM home [/swarmcc] <<
 ℹ️  Installing https://github.com/complyue/network.git to edh-universe/complyue/network ...
Cloning into 'edh-universe/complyue/network'...
remote: Enumerating objects: 7582, done.
remote: Counting objects: 100% (84/84), done.
remote: Compressing objects: 100% (56/56), done.
remote: Total 7582 (delta 31), reused 62 (delta 22), pack-reused 7498
Receiving objects: 100% (7582/7582), 1.81 MiB | 516.00 KiB/s, done.
Resolving deltas: 100% (4345/4345), done.
 ℹ️  Installed https://github.com/complyue/network.git .
[root@swarmcc /swarmcc/edh-universe]# cd complyue/network/
[root@swarmcc /swarmcc/edh-universe/complyue/network]# autoreconf
[root@swarmcc /swarmcc/edh-universe/complyue/network]# cd ../..
```

```console
[root@swarmcc /swarmcc/edh-universe]# epm x cabal install sedh:exes
 ℹ️   >> Managing packages at EPM home [/swarmcc] <<
Wrote tarball sdist to
/swarmcc/edh-universe/dist-newstyle/sdist/nedh-0.1.0.0.tar.gz
Wrote tarball sdist to
/swarmcc/edh-universe/dist-newstyle/sdist/elr-0.3.0.0.tar.gz
  ...
Symlinking 'swarmcc'
Symlinking 'forage'
Symlinking 'gwd'
[root@swarmcc /swarmcc/edh-universe]#
```

- Configure Swarm Control Center

```console
[root@swarmcc /swarmcc/edh-universe]# cd ..
[root@swarmcc /swarmcc]# mkdir etc
[root@swarmcc /swarmcc]# epm x swarmcc
 ℹ️   >> Managing packages at EPM home [/swarmcc] <<
ℹ️  /swarmcc/edh-universe/e-wrks/nedh/edh_modules/net/repl.edh:18:24
Đ (Edh) web REPL listening: ws://127.0.0.1:2721
ℹ️  /swarmcc/edh-universe/e-wrks/nedh/edh_modules/net/repl.edh:41:20
Đ (Edh) web REPL listening: http://127.0.0.1:2714
ℹ️  /swarmcc/edh-universe/e-wrks/sedh/edh_modules/swarm/cc/server.edh:93:24
Swarm Control Center listening: http://0.0.0.0:6780
ℹ️  /swarmcc/edh-universe/e-wrks/sedh/edh_modules/swarm/cc/server.edh:106:24
Swarm Control Center sniffing udp://0.0.0.0:6768 for cnode heartbeating.
^CEdh program crashed with an error:

💔 traceback
📜 module:swarm/cc 👉 /swarmcc/edh_modules/swarm/cc/__main__.edh:7:1-7:11
📜 join 👉 /swarmcc/edh-universe/e-wrks/sedh/edh_modules/swarm/cc/server.edh:233:5-233:21
🛑 Ctrl^C pressed

[root@swarmcc /swarmcc]# 

```

```console

```

```console

```

```console

```

```console

```

```console

```

```console

```
