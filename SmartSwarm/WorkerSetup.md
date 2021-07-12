# Setup Diskless Ubuntu for Swarm Workers

The worker machines (a.k.a. computing nodes) will ideally run diskless, i.e. they'll boot via PXE, into Ubuntu with no local filesystem, all filesystems of the worker os are backed by the storage server, they are remotely mounted via `NFS`, thus shared, by all worker machines of the swarm.

- [Advisories](#advisories)
- [Setup Procedure](#setup-procedure)
  - [Create Filesystems on the Controller](#create-filesystems-on-the-controller)
  - [Setup Diskless Ubuntu](#setup-diskless-ubuntu)
    - [Install a Disk-ful Ubuntu Instance](#install-a-disk-ful-ubuntu-instance)
    - [Tweak the Disk-ful Ubuntu](#tweak-the-disk-ful-ubuntu)
    - [Upload & Tweak Files for the Diskless Instance](#upload--tweak-files-for-the-diskless-instance)
    - [Update the Controller for this Diskless System](#update-the-controller-for-this-diskless-system)
  - [Setup the Diskless System for Swarm Work](#setup-the-diskless-system-for-swarm-work)
    - [Create the Worker Account](#create-the-worker-account)
    - [Install Haskell Stack](#install-haskell-stack)
    - [Install Sedh and ELS](#install-sedh-and-els)
    - [Configure Heartbeating](#configure-heartbeating)
    - [Configure Swarm Forager](#configure-swarm-forager)
  - [Done](#done)

## Advisories

- Just pack up RAM, CPU, and NIC with PXE boot-ability into each physical machine.

- Such worker machines (computing nodes) ideally all have same hardware topology and size.

- Setup a separate instance of the diskless system, for hardware configurations differ enough.

## Setup Procedure

### Create Filesystems on the Controller

Several `ZFS` filesystems on the controller (which assumes the storage server role) are to be created for each diskless system instance, including:

- The usually readonly root filesystem
- The usually writable `/var` filesystem
- The writable filesystem to persist swap files per worker machine
- The writable workspace to store artifacts as payload of swarm works

Login as root to `global zone` of the SmartOS<sup>TM</sup> controller machine, run the following commands:

> Note: Change the mountpoint, network address and quota/reservation per your needs

```console
zfs create -o mountpoint=/cnroot -o sharenfs=root=@10.0.0.0/8 -o quota=20G -o reservation=10G zones/cnroot
zfs create -o mountpoint=/cnvar -o sharenfs=root=@10.0.0.0/8 -o quota=20G -o reservation=10G zones/cnvar
zfs create -o mountpoint=/cnswap -o sharenfs=root=@10.0.0.0/8 -o quota=80G -o reservation=60G zones/cnswap
zfs create -o mountpoint=/cnwkspc -o sharenfs=root=@10.0.0.0/8 -o quota=80G -o reservation=60G zones/cnwkspc
```

### Setup Diskless Ubuntu

> You'll need to edit several system files with `sudo` privilege, with terminal UI. If you are comfortable with `vi`, then just use the commands demonstrated in rest of this document, or you might like to learn about [nano](https://help.ubuntu.com/community/Nano) editor, and use it in places where `vi` is stated.

You'll first have a _normal_ Ubuntu installation, then dump its files into nfs shared filesystems, and at last, tweak the files in shared filesystem to become a diskless system.

#### Install a Disk-ful Ubuntu Instance

You can either use one of your physical worker machines, or create a virtual machine, to install a Ubuntu Server instance.

> A virtual machine comes handy in usual cases. Or with one of your physical worker machines, there tends to be no disk inside at all. Then you may use a USB stick. A USB stick is the perfect media for Ubuntu installation, and if it's large enough, can also be the installation target, though file copying speed tends to be quite slow. An external HDD can come handy wrt file copying speed, so you can finish the installation faster.

Anyway, just follow https://ubuntu.com/tutorials/install-ubuntu-server

> An user account with adminstration privilege will be created during the installation process, the commands demonstrated in rest of this document will assume that admin account be named `dwa`, mind to substitute this name to your preferred one.

#### Tweak the Disk-ful Ubuntu

> Ubuntu tends to prevent direct `root` login, so you usually login with account `dwa`, who has `sudo` privilege.

Generate a default key pair for login via SSH

```console
$ ssh-keygen
Generating public/private rsa key pair.
Enter file in which to save the key (/home/dwa/.ssh/id_rsa):
Enter passphrase (empty for no passphrase):
Enter same passphrase again:
Your identification has been saved in /home/dwa/.ssh/id_rsa
Your public key has been saved in /home/dwa/.ssh/id_rsa.pub
The key fingerprint is:
SHA256:TtWZFSW4RLGJh60gfACBnT2TvaZpbn0hCp522uOYFLs dwa@cnode
The key's randomart image is:
+---[RSA 3072]----+
|   oo=.o   .oo+o.|
|  . o.=..  =o* . |
|      ooo.+.O.   |
|       ooo o.    |
|    .  +S .      |
|    .o+o. .      |
|   .o= o.. .     |
|   .+=* . .      |
|   .E=o. .       |
+----[SHA256]-----+
$ cp .ssh/id_rsa.pub .ssh/authorized_keys
$ chmod 600 .ssh/authorized_keys
```

> You can do `sudo -s` to drop current terminal session into a `root` shell, saving you from repeatedly typing `sudo` before every commands. Commands demonstrated in rest of this document assume to be executed in such `root` shell sessions unless specifically stated otherwise.

Allow `sudo` without password verification for `dwa`, this is needed by _one-click reboot_ of worker machines on the swarm management page.

```bash
echo 'dwa ALL=(ALL) NOPASSWD:ALL' > /etc/sudoers.d/dwa
```

Set a static hostname

```bash
echo cnode > /etc/hostname
```

Simplify network config into kernel dhcp, so the default network management services can be removed

```bash
vi /etc/default/grub
```

Change the line containing _GRUB_CMDLINE_LINUX_DEFAULT_ to read `GRUB_CMDLINE_LINUX_DEFAULT="ip=dhcp"`

```bash
update-grub
reboot
```

Remove network management services

```bash
systemctl stop systemd-networkd.socket systemd-networkd networkd-dispatcher systemd-networkd-wait-online
systemctl disable systemd-networkd.socket systemd-networkd networkd-dispatcher systemd-networkd-wait-online
systemctl mask systemd-networkd.socket systemd-networkd networkd-dispatcher systemd-networkd-wait-online
systemctl disable systemd-resolved
systemctl stop systemd-resolved
systemctl mask systemd-resolved

apt -y purge nplan netplan.io
```

Install additional utilities & libraries

```bash
apt install git htop bmon nfs-common uuid curl libgmp-dev libtinfo-dev
```

Configure time sync to a local time server

```bash
vi /etc/systemd/timesyncd.conf
```

Add a line `NTP=10.88.88.139` below its corresponding comment

> Use the Swarm Controller's IP, if you'd decided to use it as the time server. Or the other time server you'd prefer.

Recreate a `/etc/resolv.conf`, with the dns server of your preference

> Mind to change the IPs

```bash
rm /etc/resolv.conf
cat << EOF > /etc/resolv.conf
nameserver 223.5.5.5
nameserver 119.29.29.29
EOF
```

Make it immutable, so not overwritable by malicious bloatwares

> https://linoxide.com/how-tos/change-attributes-of-file

```bash
chattr +i /etc/resolv.conf
```

Check swap status, disable it

```bash
swap on
swapoff -v /swap.img
```

Remove swap fs, use tmpfs for log

```bash
vi /etc/fstab
```

Remove the line about swap, add a line containing `tmpfs /var/log tmpfs defaults,size=512M 0 0`

Remove swap file

```bash
rm /swap.img
```

Cleanup log files and reboot

```bash
rm -rf /var/log/*
reboot
```

#### Upload & Tweak Files for the Diskless Instance

Setup transient `nfs` mounts for filesystems on the controller (storage server)

> Replace `10.88.88.139` with the actual ip of your controller (storage server)

```bash
mkdir /run/cnroot /run/cnvar /run/cnswap
mount -t nfs -o rw,vers=3,noacl,nolock 10.88.88.139:/cnroot /run/cnroot
mount -t nfs -o rw,vers=3,noacl,nolock 10.88.88.139:/cnvar /run/cnvar
mount -t nfs -o rw,vers=3,noacl,nolock 10.88.88.139:/cnswap /run/cnswap
```

Upload files to the shared filesystems

> This may take a while

```bash
cp -ax /. /run/cnroot/.
cp -ax /dev/. /run/cnroot/dev/.
cp -ax /var/. /run/cnvar/.
```

Remove junk files there

```bash
rmdir /run/cnroot/lost+found
rm -rf /run/cnroot/tmp
rm -rf /run/cnroot/var
rm -rf /run/cnroot/etc/netplan/*
rm /run/cnroot/etc/hostname
rm -rf /run/cnvar/log
mkdir /run/cnvar/log
```

Re-create essential files there

```bash
mkdir /run/cnroot/tmp /run/cnroot/var /run/cnroot/cnswap
ln -s /run /run/cnroot/var/run
ln -s /run/lock /run/cnroot/var/lock
```

Re-create `/etc/fstab` for the shared system root

> Replace `10.88.88.139` with the actual ip of your controller (storage server)

```bash
cat << EOF > /run/cnroot/etc/fstab
10.88.88.139:/cnroot  /          nfs     ro,vers=3,noacl,nolock  0 0
tmpfs                 /tmp       tmpfs   defaults,size=2G        0 0
10.88.88.139:/cnwkspc /workspace nfs     rw,vers=3,noacl,nolock  0 0
EOF
```

Prepare `initramfs` for netboot

```bash
cd /run/cnroot/boot
rm -rf config-* efi/ grub/
vi /run/cnroot/etc/initramfs-tools/initramfs.conf
```

Change `MODULES=most` to `MODULES=netboot`, save the file, then generate a netboot `initrd` on the shared root

```bash
mkinitramfs -d /run/cnroot/etc/initramfs-tools -o /run/cnroot/boot/initrd.img-$(uname -r)
```

Create symbolic links for kernel/initrd to be netbooted, this'll be used in your pixie boot config, and you can update the symbolic links here to switch to updated kernel/initrd in the future.

```bash
ln -s vmlinuz-$(uname -r) kernel
ln -s initrd.img-$(uname -r) initrd
```

Config `/var` and `/var/log` filesystem

> Replace `10.88.88.139` with the actual ip of your controller (storage server)

```bash
cat << EOF > /run/cnroot/etc/systemd/system/var.mount
[Unit]
Description=Compute Node's var fs over NFS
After=network.target
Before=var-log.mount

[Mount]
What=10.88.88.139:/cnvar
Where=/var
Type=nfs
Options=rw,vers=3,noacl,nolock

[Install]
WantedBy=multi-user.target
EOF
```

```bash
cat << EOF > /run/cnroot/etc/systemd/system/var-log.mount
[Unit]
Description=Transient log fs
After=var.mount
Before=syslogd.service rsyslogd.service

[Mount]
Where=/var/log
What=tmpfs
Type=tmpfs
Options=size=512M

[Install]
WantedBy=multi-user.target
EOF
```

Config swap over nfs

```bash
mdir /run/cnswap/sbin

cat << EOF > /run/cnswap/sbin/cn-set-swap.sh
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
EOF
```

```bash
cat << EOF > /run/cnswap/sbin/cn-unset-swap.sh
#!/bin/bash

swapoff /dev/loop0
losetup -d /dev/loop0
echo "*off*" >> /run/cn-swapfile
EOF

chmod +x /run/cnswap/sbin/*.sh
```

```bash
cat << EOF > /run/cnroot/etc/systemd/system/cnswap.mount
[Unit]
Description=Compute Node swap Root
After=network.target

[Mount]
What=10.88.88.139:/cnswap
Where=/cnswap
Type=nfs
Options=rw,vers=3,noacl,nolock

[Install]
WantedBy=multi-user.target
EOF
```

```bash
cat << EOF > /run/cnroot/etc/systemd/system/cnswap.service
[Unit]
Description=Compute Node swap Setup
Requires=cnswap.mount

[Service]
Type=oneshot
WorkingDirectory=/cnswap
ExecStart=/cnswap/sbin/cn-set-swap.sh
RemainAfterExit=true
ExecStop=/cnswap/sbin/cn-unset-swap.sh
User=root
Group=root

[Install]
WantedBy=multi-user.target
EOF
```

```bash
ln -s /etc/systemd/system/var.mount /run/cnroot/etc/systemd/system/multi-user.target.wants/var.mount
ln -s /etc/systemd/system/var-log.mount /run/cnroot/etc/systemd/system/multi-user.target.wants/var-log.mount
ln -s /etc/systemd/system/cnswap.mount /run/cnroot/etc/systemd/system/multi-user.target.wants/cnswap.mount
ln -s /etc/systemd/system/cnswap.service /run/cnroot/etc/systemd/system/multi-user.target.wants/cnswap.service
```

Lift limits

```bash
cat << EOF > /run/cnroot/etc/security/limits.d/maxfiles.conf
* soft nofile 30000
* hard nofile 30000
EOF
```

```bash
cat << EOF > /run/cnroot/etc/sysctl.d/90-inotif_user_watches.conf
fs.inotify.max_user_watches = 30000
EOF

```

#### Update the Controller for this Diskless System

Login as root to `global zone` of the SmartOS<sup>TM</sup> controller machine, run the following commands to add the root filesystem available to the controller zone

```console
cat << EOF > add-cnroot.json
> {
>   "add_filesystems": [
>     {
>       "source": "/cnroot",
>       "target": "/cnroot",
>       "type": "lofs"
>     }
>   ]
> }
> EOF

vmadm update 06536e48-ae04-44a8-8996-f5d7a39612a4 -f add-cnroot.json

vmadm reboot 06536e48-ae04-44a8-8996-f5d7a39612a4
```

Configure the controller's service account to use this system's administrator's ssh key for login, this is necessary for _one-click reboot_ of worker machines (on the management page) to work correctly

```console
# zlogin 06536e48-ae04-44a8-8996-f5d7a39612a4
[Connected to zone '06536e48-ae04-44a8-8996-f5d7a39612a4' pts/3]
Last login: Thu Jul  8 16:04:07 on pts/3
   __        .                   .
 _|  |_      | .-. .  . .-. :--. |-
|_    _|     ;|   ||  |(.-' |  | |
  |__|   `--'  `-' `;-| `-' '  ' `-'
                   /  ; Instance (minimal-64-lts 20.4.0)
                   `-'  https://docs.joyent.com/images/smartos/minimal

[root@swarmcc ~]# echo 'IdentityFile /cnroot/home/dwa/.ssh/id_rsa' >> ~/.ssh/config
```

Optionally, make this diskless system the default

> An unknown worker machine will boot into the default system as configured in `etc/default.edh`

```console
[root@swarmcc ~]# cd /swarmcc/etc/
[root@swarmcc /swarmcc/etc]# vi default.edh
```

Adjust `nfs_server = '10.88.88.139'` to reflect your actual controller's ip, as well for other options as appropriate

```console
[root@swarmcc /swarmcc/etc]# cat default.edh
# ctrl fields as well as metadata
swarm = 'cn'
user = 'dw'
admin = 'dwa'

# this is always implanted automatically
# note: use (=:) to just prototype it, but never overwrite it!
mac =: '00:00:00:00:00:00'

# these should be continuously updated by heartbeats from the booted node
# note: use (?=) to supply initial values, but never overwrite existing ones!
timestamp ?= None
ip ?= None
vmem ?= {"total": 0, "available": 0, "active": 0, "inactive": 0, }
swap ?= {"total": 0, "used": 0, "free": 0,}
cpuload ?= [] # utilized percents of each virtual one
nps ?= 0 # number of processes

# provide clickable link to show on the browser for login to this node
method login_link() {
  null(ip) -> 'javascript:alert("Compute node ['++ mac ++'] not booted!")'
  'ssh://' ++ user ++ '@' ++ ip
}

method admin_account() {
  admin ++ '@' ++ ip
}

# make it bootable or not, mounting root fs from certain nfs server
bootable = true
nfs_server = '10.88.88.139'
if bootable then perform boot(
  kernel= "file:///"++ swarm ++"root/boot/kernel",
  initrd= [ "file:///"++ swarm ++"root/boot/initrd", ],
  cmdline= ' '.join(
    "root=/dev/nfs",
    "nfsroot="++ nfs_server ++":/"++ swarm ++"root,ro,noacl",
    "ip=dhcp",
  )
)
```

### Setup the Diskless System for Swarm Work

Boot a worker machine, login as `dwa`, and `sudo -s` then:

Make the root filesystem writable on this machine (until rebooted)

```bash
mount -o remount,rw /
```

Disable unnecessary services

```bash
systemctl disable apparmor
systemctl mask apparmor
systemctl disable grub-initrd-fallback
systemctl mask grub-initrd-fallback
```

#### Create the Worker Account

> The commands demonstrated below uses user name `dw`, change per your preference.

This user account is granted `sudo` privilege, and takes `/workspace` as home.

Also configure `~/.local/bin` directory for this user, as part of its `PATH` env, and install [EPM](https://github.com/e-wrks/epm) there.

```bash
useradd -d /workspace -g dwa dw
echo 'dw ALL=(ALL) NOPASSWD:ALL' > /etc/sudoers.d/dw

mkdir /workspace/.ssh
cp /home/dwa/.ssh/authorized_keys /workspace/.ssh/authorized_keys

mkdir -p /workspace/.local/bin
echo 'PATH=/workspace/.local/bin:$PATH' > /workspace/.profile

curl -o /workspace/.local/bin/epm -L https://github.com/e-wrks/epm/raw/latest/epm
chmod +x /workspace/.local/bin/epm

chown -R dw:dwa /workspace
```

Optionally set a password.

```console
# passwd dw
New password:
Retype new password:
passwd: password updated successfully
```

Login with this newly created worker account (i.e. `ssh dw@<worker-ip>`)

```console
dw@cnode:~$ pwd
/workspace
dw@cnode:~$
```

#### Install Haskell Stack

See [the docs](https://docs.haskellstack.org) for reference.

```bash
curl -sSL https://get.haskellstack.org/ | sh
stack update
```

#### Install Sedh and ELS

Make `/workspace` an epm home, install _sedh_ as well _els_ there with dependencies

> _sedh_ builds executables for swarm services, esp. the swarm work forager

> _els_ will provide IntelliSense when later you have [VSCode](https://code.visualstudio.com) connect to worker machines vis SSH, for development & work management etc.

```bash
cd /workspace
epm init
epm install edh nedh sedh els
```

```console
cd /workspace/edh-universe/
stack install
```

#### Configure Heartbeating

```bash
echo '* * * * * /workspace/edh-universe/bin/rptup.py' > rptup.cron
crontab rptup.cron
```

#### Configure Swarm Forager

> Modify `/workspace/edh_modules/forage/__main__.edh` to set `headcount` matching number of physical CPU cores in every worker machine of your swarm, other parameters can be adjusted as well

```console
dw@cnode:~$ cat /workspace/edh_modules/forage/__main__.edh
{## Entry module of an Đ Swarm Forager #}

import * 'swarm/forager'

forageOn(
  heartcount= 1, # number of worker processes to vend
  targetPrefix = '', # prefix to filter out uninteresting work
  swarmAddr = '0.0.0.0', # local addr to sniff for call-for-workers
  swarmPort = 3722, # local port to sniff for call-for-workers
).join()
dw@cnode:~$
```

Start a `root` terminal session with `sudo -s`, config the `forager` systemd service by running following commands:

```bash
cat << EOF > /etc/systemd/system/forager.service
[Unit]
Description=Swarm Forager
Requires=

[Service]
User=dw
Group=dwa
CapabilityBoundingSet=CAP_NET_BIND_SERVICE
AmbientCapabilities=CAP_NET_BIND_SERVICE

StartLimitInterval=5
StartLimitBurst=10

WorkingDirectory=/workspace
ExecStart=/workspace/.local/bin/epm exec forage

Restart=always
RestartSec=120

[Install]
WantedBy=multi-user.target
EOF
```

```bash
systemctl daemon-reload
systemctl enable forager
systemctl start forager
```

### Done
