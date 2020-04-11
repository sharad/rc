#!/bin/sh

function running()
{
    local _cmd=$2
    shift

    echo $_cmd "$@"
    $_cmd "$@"
}


CONFIG=$1

if [ x = "x$CONFIG" ]
then
    echo config file not supplied as first argument >&2
    exit -1
fi

SYSTEM_INIT="$(grep 'define %lotus-system-init' $CONFIG | cut -d' ' -f3 | cut -c2)"

if [ "$SYSTEM_INIT" = "f" ]
then
    echo %lotus-system-init is set false, set it to true >&2
    exit -1
fi

umount /mnt/boot/efi
umount /mnt/boot/
umount /mnt/tmp
umount /mnt/var
umount /mnt/gnu
umount /mnt

vgchange -ay guix

sleep 2

if [ -r $CONFIG ]
then
    if [ -e /dev/mapper/guix-root ]
    then
        if [ -e /dev/mapper/guix-gnu ]
        then

            mkfs.ext4 /dev/mapper/guix-root
            mkfs.ext4 /dev/mapper/guix-boot
            mkfs.ext4 /dev/mapper/guix-var
            mkfs.ext4 /dev/mapper/guix-tmp
            mkfs.ext4 /dev/mapper/guix-gnu

        else
            echo /dev/mapper/guix-gnu not exists
        fi                          # if [ -e /dev/mapper/guix-gnu ]
    else
        echo /dev/mapper/guix-root not exists
    fi                              # if [ -e /dev/mapper/guix-root ]
else
    echo $CONFIG not exists
fi
