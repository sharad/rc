#!/bin/sh

function running()
{
    local _cmd=$2
    shift

    echo $_cmd "$@"
    $_cmd "$@"
}

##
CONFIG="$1"
DISK_SERIAL_ID="$2"
# UUID=A198-A7BC or /dev/sdX1
# blkid /dev/sdx1
EFI_DISK="$3"

if [ x = "x${CONFIG}" ]
then
    echo config file not supplied as first argument >&2
    exit -1
fi

if [ -r "${CONFIG}" ]
then
    echo file "${CONFIG}" not exists >&2
    exit -1
fi


if [ x = "x${DISK_SERIAL_ID}" ]
then
    echo disk serial id not supplied. >&2
    exit -1
fi

if [ x = "x${EFI_DISK}" ]
then
    echo efi disk is not supplied as thrid argument >&2
    exit -1
fi

DEV_ROOT="/dev/mapper/${DISK_SERIAL_ID}guix-root"
DEV_BOOT="/dev/mapper/${DISK_SERIAL_ID}guix-boot"
DEV_VAR="/dev/mapper/${DISK_SERIAL_ID}guix-var"
DEV_TMP="/dev/mapper/${DISK_SERIAL_ID}guix-tmp"
DEV_GNU="/dev/mapper/${DISK_SERIAL_ID}guix-gnu"

for part in "${DEV_ROOT}" "${DEV_BOOT}" "${DEV_VAR}" "${DEV_TMP}" "${DEV_GNU}"
do
    echo echcking disk file = "$part"
    if [ -e "$part" ]
    then
        echo file "$part" not exists. >&2
        exit -1
    fi
    if [ -b "$part" ]
    then
        echo file "$part" is not block device. >&2
        exit -1
    fi
done
##

SYSTEM_INIT="$(grep 'define %lotus-system-init' ${CONFIG} | cut -d' ' -f3 | cut -c2)"

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

vgchange -ay "${DISK_SERIAL_ID}guix"

sleep 2

if [ -r "${CONFIG}" ]
then
    if [ -e "${DEV_ROOT}" ]
    then
        if [ -e "${DEV_GNU}" ]
        then

            if mount "${DEV_ROOT}" /mnt
            then
                mkdir -p /mnt/gnu /mnt/etc /mnt/tmp /mnt/boot/efi /mnt/var

                if mount "${DEV_GNU}" /mnt/gnu         &&
                        mount "${DEV_TMP}" /mnt/tmp    &&
                        mount "${DEV_VAR}" /mnt/var    &&
                        mount "${DEV_BOOT}" /mnt/boot/ &&
                        mkdir -p /mnt/boot/efi                 &&
                        mount /dev/sda1 /mnt/boot/efi

                then

                        mount "${DEV_BOOT}" /mnt/boot/
                        mount "${DEV_TMP}" /mnt/tmp
                        mount "${DEV_VAR}" /mnt/var
                        mkdir -p /mnt/boot/efi
                        mount /dev/sda1 /mnt/boot/efi

                        df -hT

                else                # mount "${DEV_GNU}" /mnt/gnu
                    echo can not mount "${DEV_GNU}" /mnt/gnu
                fi                  # mount "${DEV_GNU}" /mnt/gnu
            else
                echo can not mount "${DEV_ROOT}" /mnt
            fi                      # if mount "${DEV_ROOT}" /mnt
        else
            echo "${DEV_GNU}" not exists
        fi                          # if [ -e "${DEV_GNU}" ]
    else
        echo "${DEV_ROOT}" not exists
    fi                              # if [ -e "${DEV_ROOT}" ]
else
    echo "${CONFIG}" not exists
fi
