#!/bin/sh

CONFIG=$1

vgchange -ay guix

sleep 2

if [ -r $CONFIG ]
then
    if [ -e /dev/mapper/guix-root ]
    then
        mkfs.ext4 /dev/mapper/guix-root
        if [ -e /dev/mapper/guix-store ]
        then
            mkfs.ext4 /dev/mapper/guix-store


            if mount /dev/mapper/guix-root /mnt
            then
                mkdir -p /mnt/gnu /mnt/etc

                if mount /dev/mapper/guix-store /mnt/gnu
                then
                    cp "$CONFIG" /mnt/etc/config.scm

                    echo herd start cow-store /mnt

                    herd start cow-store /mnt

                    echo sleep 3

                    sleep 3

                    echo guix system init /mnt/etc/config.scm /mnt/

                    guix system init /mnt/etc/config.scm /mnt/

                else                # mount /dev/mapper/guix-store /mnt/gnu
                    echo can not mount /dev/mapper/guix-store /mnt/gnu
                fi                  # mount /dev/mapper/guix-store /mnt/gnu
            else
                echo can not mount /dev/mapper/guix-root /mnt
            fi                      # if mount /dev/mapper/guix-root /mnt
        else
            echo /dev/mapper/guix-store not exists
        fi                          # if [ -e /dev/mapper/guix-store ]
    else
        echo /dev/mapper/guix-root not exists
    fi                              # if [ -e /dev/mapper/guix-root ]
else
    echo $CONFIG not exists
fi
