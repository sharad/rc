#!/bin/sh

CONFIG=$1

if [ x = "x$CONFIG" ]
then
    echo config file not supplied as first argument >&2
    exit -1
fi

umount /mnt/boot/efi
umount /mnt/boot/
umount /mnt/tmp
umount /mnt/gnu
umount /mnt

vgchange -ay guix

sleep 2

if [ -r $CONFIG ]
then
    if [ -e /dev/mapper/guix-root ]
    then
        mkfs.ext4 /dev/mapper/guix-root
        if [ -e /dev/mapper/guix-gnu ]
        then
            mkfs.ext4 /dev/mapper/guix-tmp
            mkfs.ext4 /dev/mapper/guix-gnu


            if mount /dev/mapper/guix-root /mnt
            then
                mkdir -p /mnt/gnu /mnt/etc /mnt/tmp /mnt/boot/efi

                if mount /dev/mapper/guix-gnu /mnt/gnu &&
                        mount /dev/mapper/guix-tmp /mnt/tmp &&
                        mount /dev/mapper/guix-boot /mnt/boot/ &&
                        mount /dev/sda1 /mnt/boot/efi
                then
                    cp "$CONFIG" /mnt/etc/config.scm

                    echo herd start cow-store /mnt

                    herd start cow-store /mnt

                    echo sleep 3

                    sleep 3

                    echo guix system init /mnt/etc/config.scm /mnt/

                    guix system init /mnt/etc/config.scm /mnt/

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
