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

if [ ! -r "${CONFIG}" ]
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

DEV_ROOT="/dev/mapper/${DISK_SERIAL_ID}Xguix-root"
DEV_BOOT="/dev/mapper/${DISK_SERIAL_ID}Xguix-boot"
DEV_VAR="/dev/mapper/${DISK_SERIAL_ID}Xguix-var"
DEV_TMP="/dev/mapper/${DISK_SERIAL_ID}Xguix-tmp"
DEV_GNU="/dev/mapper/${DISK_SERIAL_ID}Xguix-gnu"

for part in "${DEV_ROOT}" "${DEV_BOOT}" "${DEV_VAR}" "${DEV_TMP}" "${DEV_GNU}"
do
    echo checking disk file = "$part"
    if [ ! -e "$part" ]
    then
        echo file "$part" not exists. >&2
        exit -1
    fi
    if [ ! -b "$part" ]
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

vgchange -ay ${DISK_SERIAL_ID}Xguix

sleep 2

if [ -r ${CONFIG} ]
then
    if [ -e "${DEV_ROOT}" ]
    then
        if [ -e "${DEV_GNU}" ]
        then

            mkfs.ext4 "${DEV_ROOT}"
            mkfs.ext4 "${DEV_BOOT}"
            mkfs.ext4 "${DEV_VAR}"
            mkfs.ext4 "${DEV_TMP}"
            mkfs.ext4 "${DEV_GNU}"

        else
            echo "${DEV_GNU}" not exists
        fi                          # if [ -e "${DEV_GNU}" ]
    else
        echo "${DEV_ROOT}" not exists
    fi                              # if [ -e "${DEV_ROOT}" ]
else
    echo "${CONFIG}" not exists
fi

