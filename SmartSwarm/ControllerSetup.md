# Setup a Swarm Controller running SmartOS<sup>TM</sup>

- [Advisories](#advisories)
  - [Hardware Planning](#hardware-planning)
  - [Plenty RAM & Decent CPU](#plenty-ram--decent-cpu)
  - [RAID be just Mirror](#raid-be-just-mirror)
  - [USB Stick Booting](#usb-stick-booting)
- [Setup Procedure](#setup-procedure)
  - [OS Install](#os-install)
  - [Advised Performance Tweaks](#advised-performance-tweaks)
  - [(Optional) Turn the Controller into an NTP Server](#optional-turn-the-controller-into-an-ntp-server)
  - [Setup a Zone for Swarm Control Center](#setup-a-zone-for-swarm-control-center)
  - [Done](#done)

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

### (Optional) Turn the Controller into an NTP Server

All machines of the swarm should ideally have their system time closely sync'ed, and for HPC scenario, you'd better have a local time server instead of one on the internet. Though a bit hacky, SmartOS can be tweaked to assuming a time server role.

```console
[root@smartvm ~]# cd /usbkey
[root@smartvm /usbkey]# mkdir config.inc
[root@smartvm /usbkey]# cp /etc/inet/ntp.conf config.inc/
[root@smartvm /usbkey]# vi config.inc/ntp.conf
[root@smartvm /usbkey]# diff /etc/inet/ntp.conf config.inc/ntp.conf
0a1,5
> # add a line `ntp_conf_file=ntp.conf` to /usbkey/config
> # so this file (/usbkey/config.inc/ntp.conf) is copied
> # to /etc/inet/ntp.conf at each boot
>
>
5,6c10,12
< restrict default ignore
< restrict -6 default ignore
---
> # NO, don't ignore, so this node serves as an NTP server
> #restrict default ignore
> #restrict -6 default ignore
[root@smartvm /usbkey]# echo 'ntp_conf_file=ntp.conf' >> /usbkey/config
[root@smartvm /usbkey]# reboot
```

### Setup a Zone for Swarm Control Center

- Create a filesystem

> The `Swarm Control Center` service will run in a native zone, this separate ZFS filesystem will be used to store its software components & configurations

```console
zfs create -o mountpoint=/swarmcc -o quota=2G -o reservation=100M zones/swarmcc
```

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
[root@swarmcc ~]# pkgin in git gcc9 ghc cabal-install go automake
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

- Install bleeding edge of `network` to workaround the compilation errors

> https://github.com/haskell/network/issues/507 has been merged, you don't need to do this once the next release on Hackage got it included

```console
[root@swarmcc /swarmcc/edh-universe]# epm i https://github.com/haskell/network.git
 ℹ️   >> Managing packages at EPM home [/swarmcc] <<
 ℹ️  Installing https://github.com/haskell/network.git to edh-universe/haskell/network ...
Cloning into 'edh-universe/haskell/network'...
remote: Enumerating objects: 7582, done.
remote: Counting objects: 100% (84/84), done.
remote: Compressing objects: 100% (56/56), done.
remote: Total 7582 (delta 31), reused 62 (delta 22), pack-reused 7498
Receiving objects: 100% (7582/7582), 1.81 MiB | 516.00 KiB/s, done.
Resolving deltas: 100% (4345/4345), done.
 ℹ️  Installed https://github.com/haskell/network.git .
[root@swarmcc /swarmcc/edh-universe]# cd haskell/network/
[root@swarmcc /swarmcc/edh-universe/haskell/network]# autoreconf
[root@swarmcc /swarmcc/edh-universe/haskell/network]# cd ../..
```

```console
[root@swarmcc /swarmcc/edh-universe]# epm x cabal install sedh
 ℹ️   >> Managing packages at EPM home [/swarmcc] <<
Wrote tarball sdist to
/swarmcc/edh-universe/dist-newstyle/sdist/nedh-0.1.0.0.tar.gz
Wrote tarball sdist to
/swarmcc/edh-universe/dist-newstyle/sdist/elr-0.3.0.0.tar.gz
Wrote tarball sdist to
/swarmcc/edh-universe/dist-newstyle/sdist/edh-0.3.0.0.tar.gz
Wrote tarball sdist to
/swarmcc/edh-universe/dist-newstyle/sdist/lossless-decimal-0.3.0.0.tar.gz
Wrote tarball sdist to
/swarmcc/edh-universe/dist-newstyle/sdist/sedh-0.1.0.0.tar.gz
Wrote tarball sdist to
/swarmcc/edh-universe/dist-newstyle/sdist/network-3.1.2.2.tar.gz
Resolving dependencies...
  ...
Symlinking 'gwd'
Symlinking 'swarmcc'
Symlinking 'forage'
[root@swarmcc /swarmcc/edh-universe]#
```

- Configure Swarm Control Center

```console
[root@swarmcc /swarmcc/edh-universe]# cd ..
[root@swarmcc /swarmcc]# mkdir etc
[root@swarmcc /swarmcc]# cat smf/swarm/cc.xml
<?xml version='1.0'?>
<!DOCTYPE service_bundle SYSTEM '/usr/share/lib/xml/dtd/service_bundle.dtd.1'>
<service_bundle type='manifest' name='swarmcc'>
  <service name='swarm/cc' type='service' version='0'>
    <create_default_instance enabled='true' />
    <single_instance />
    <dependency name='fs' grouping='require_all' restart_on='none' type='service'>
      <service_fmri value='svc:/system/filesystem/local' />
    </dependency>
    <dependency name='net' grouping='require_all' restart_on='none' type='service'>
      <service_fmri value='svc:/network/loopback' />
    </dependency>
    <exec_method name='start' type='method' exec="bash -c 'pkill swarmcc; ctrun /opt/local/bin/epm x swarmcc &amp;'" timeout_seconds='60'>
      <method_context working_directory='/swarmcc'>
        <method_credential user='root' group='root' />
        <method_environment>
          <envvar name='LANG' value='C.UTF-8' />
          <envvar name='EDH_LOG_LEVEL' value='INFO' />
        </method_environment>
      </method_context>
    </exec_method>
    <exec_method name='stop' type='method' exec='pkill swarmcc' timeout_seconds='60'>
      <method_context />
    </exec_method>
  </service>
</service_bundle>

[root@swarmcc /swarmcc]# svccfg import smf/swarm/cc.xml
[root@swarmcc /swarmcc]# svcadm enable cc
[root@swarmcc /swarmcc]# svcs -x cc
svc:/swarm/cc:default (?)
 State: online since Thu Jul  8 08:58:22 2021
   See: /var/svc/log/swarm-cc:default.log
Impact: None.
[root@swarmcc /swarmcc]#
```

- Install & configure Pixiecore

  https://github.com/danderson/netboot/tree/master/pixiecore

```console
[root@swarmcc /swarmcc]# mkdir bin
[root@swarmcc /swarmcc]# GOBIN=/swarmcc/bin go get go.universe.tf/netboot/cmd/pixiecore
go: downloading go.universe.tf/netboot v0.0.0-20210617221821-fc2840fa7b05
  ...
go: downloading golang.org/x/net v0.0.0-20200114155413-6afb5195e5aa
[root@swarmcc /swarmcc]#
[root@swarmcc /swarmcc]# cat smf/swarm/booter.xml
<?xml version='1.0'?>
<!DOCTYPE service_bundle SYSTEM '/usr/share/lib/xml/dtd/service_bundle.dtd.1'>
<service_bundle type='manifest' name='booter'>
  <service name='swarm/booter' type='service' version='0'>
    <create_default_instance enabled='true' />
    <single_instance />
    <dependency name='fs' grouping='require_all' restart_on='none' type='service'>
      <service_fmri value='svc:/system/filesystem/local' />
    </dependency>
    <dependency name='net' grouping='require_all' restart_on='none' type='service'>
      <service_fmri value='svc:/network/loopback' />
    </dependency>
    <dependent name='m3c3' restart_on='none' grouping='optional_all'>
      <service_fmri value='svc:/milestone/multi-user' />
    </dependent>
    <exec_method name='start' type='method' exec="bash -c 'ctrun /swarmcc/bin/pixiecore api http://127.0.0.1:6780/pixie &amp;'" timeout_seconds='60'>
      <method_context working_directory='/swarmcc'>
        <method_credential user='root' group='root' />
        <method_environment>
          <envvar name='LANG' value='C.UTF-8' />
        </method_environment>
      </method_context>
    </exec_method>
    <exec_method name='stop' type='method' exec='pkill pixiecore' timeout_seconds='60'>
      <method_context />
    </exec_method>
  </service>
</service_bundle>

[root@swarmcc /swarmcc]# svccfg import smf/swarm/booter.xml
[root@swarmcc /swarmcc]# svcadm enable booter
[root@swarmcc /swarmcc]# svcs -x booter
svc:/swarm/booter:default (?)
 State: online since Thu Jul  8 09:20:34 2021
   See: /var/svc/log/swarm-booter:default.log
Impact: None.
[root@swarmcc /swarmcc]#
```

### Done

Check the services are online

```console
[root@swarmcc /swarmcc]# svcs -x cc booter
svc:/swarm/cc:default (?)
 State: online since Thu Jul  8 08:58:22 2021
   See: /var/svc/log/swarm-cc:default.log
Impact: None.

svc:/swarm/booter:default (?)
 State: online since Thu Jul  8 09:22:59 2021
   See: /var/svc/log/swarm-booter:default.log
Impact: None.
[root@swarmcc /swarmcc]#
```
