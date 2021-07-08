# Setup Diskless Ubuntu for Swarm Workers

The worker machines (a.k.a. computing nodes) can ideally run diskless, i.e. they'll boot via PXE, into Ubuntu with no local filesystem, all filesystems of the worker os are backed by the storage server, they are remotely mounted via `NFS`, thus shared, by all worker machines of the swarm.

- [Advisories](#advisories)
- [Setup Procedure](#setup-procedure)
  - [Create Filesystems on the Controller](#create-filesystems-on-the-controller)
  - [Setup Diskless Ubuntu](#setup-diskless-ubuntu)
    - [Install a Disk-ful Ubuntu Instance](#install-a-disk-ful-ubuntu-instance)
    - [Tweak the Disk-ful Ubuntu](#tweak-the-disk-ful-ubuntu)
    - [Upload & Tweak Files for the Diskless Instance](#upload--tweak-files-for-the-diskless-instance)
    - [Make Shared Root Filesystem Available to Booter](#make-shared-root-filesystem-available-to-booter)

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

#### Tweak the Disk-ful Ubuntu

> Ubuntu tends to prevent direct `root` login, so you usually login with a normal user account, though with `sudo` privilege. You can do `sudo -s` to drop current terminal session into a `root` shell, saving you from repeatedly typing `sudo` before every commands. Commands demonstrated in rest of this document assume to be executed in such `root` shell sessions unless specifically stated otherwise.

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

Install additional utilities

```bash
apt install git htop bmon nfs-common uuid curl
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
mkdir /run/cnroot /run/cnvar
mount -t nfs -o rw,vers=3,noacl,nolock 10.88.88.139:/cnroot /run/cnroot
mount -t nfs -o rw,vers=3,noacl,nolock 10.88.88.139:/cnvar /run/cnvar
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
vi /etc/initramfs-tools/initramfs.conf
```

Change `MODULES=most` to `MODULES=netboot`, save the file, then generate a netboot `initrd` on the shared root

```bash
mkinitramfs -o /run/cnroot/boot/initrd.img-$(uname -r)
```

Restore local `initramfs` config

```bash
vi /etc/initramfs-tools/initramfs.conf
```

Change `MODULES=netboot` back to `MODULES=most`

Create symbolic links for kernel/initrd to be netbooted, this'll be used in your pixie boot config, and you can update the symbolic links here to switch to updated kernel/initrd in the future.

```bash
ln -s vmlinuz-$(uname -r) kernel
ln -s initrd.img-$(uname -r) initrd
```

```bash

```

```bash

```

```bash

```

```bash

```

```bash

```

```bash

```

```bash

```

```bash

```

```bash

```

```bash

```

```bash

```

#### Make Shared Root Filesystem Available to Booter

Login as root to `global zone` of the SmartOS<sup>TM</sup> controller machine, run the following commands:

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
