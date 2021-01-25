#!/bin/sh

function running()
{
    local _cmd=$2
    shift

    echo $_cmd "$@"
    $_cmd "$@"
}


CONFIG="$1"
DISK_SERIAL_ID="$2"

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


mkdir -p /mnt/boot/efi
mkdir -p /mnt/boot/
mkdir -p /mnt/tmp
mkdir -p /mnt/var
mkdir -p /mnt/gnu
mkdir -p /mnt

vgchange -ay guix

sleep 2

if [ -r $CONFIG ]
then
    if [ -e /dev/mapper/guix-root ]
    then
        if [ -e /dev/mapper/guix-gnu ]
        then

            if mount /dev/mapper/guix-root /mnt
            then
                mkdir -p /mnt/gnu /mnt/etc /mnt/tmp /mnt/boot/efi /mnt/var

                if mount /dev/mapper/guix-gnu /mnt/gnu         &&
                        mount /dev/mapper/guix-tmp /mnt/tmp    &&
                        mount /dev/mapper/guix-var /mnt/var    &&
                        mount /dev/mapper/guix-boot /mnt/boot/ &&
                        mkdir -p /mnt/boot/efi                 &&
                        mount /dev/sda1 /mnt/boot/efi

                then

                        mount /dev/mapper/guix-boot /mnt/boot/
                        mount /dev/mapper/guix-tmp /mnt/tmp
                        mount /dev/mapper/guix-var /mnt/var
                        mkdir -p /mnt/boot/efi
                        mount /dev/sda1 /mnt/boot/efi

                        df -hT

                else                # mount /dev/mapper/guix-gnu /mnt/gnu
                    echo can not mount /dev/mapper/guix-gnu /mnt/gnu
                fi                  # mount /dev/mapper/guix-gnu /mnt/gnu
            else
                echo can not mount /dev/mapper/guix-root /mnt
            fi                      # if mount /dev/mapper/guix-root /mnt
        else
            echo /dev/mapper/guix-gnu not exists
        fi                          # if [ -e /dev/mapper/guix-gnu ]
    else
        echo /dev/mapper/guix-root not exists
    fi                              # if [ -e /dev/mapper/guix-root ]
else
    echo $CONFIG not exists
fi
