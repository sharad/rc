#!/usr/bin/env bash

# https://linuxconfig.org/how-to-create-multiboot-usb-with-linux

DEBUG=1

function main()
{
    trap setup_finish EXIT SIGINT SIGTERM
    local CMDARGS=()
    running debug process_arg $*
    mboousb_setup ${CMDARGS[*]}
    info Finished $pgm
}

function mboousb_setup()
{
    export USBDEV=$1
    export WSPACEDIR=$2
    info CMDARGS= $*
    if [ "x$USBDEV" = "x" ]
    then
        error first arg USBDEV missing
    fi
    if [ "x$WSPACEDIR" = "x" ]
    then
        error second arg WSPACEDIR missing
    fi
    if [ ! -d "$WSPACEDIR" ]
    then
        error WSPACEDIR=$WSPACEDIR dir not exists
    fi
    if [ -e "$USBDEV" -a -b "$USBDEV" -a -d "$WSPACEDIR" ]
    then
        info $USBDEV exists
        FACTOR=2048000
        if BLKSIZE_UNFACTORED="$(sudo blockdev --getsize $USBDEV)"
        then
            info BLKSIZE_UNFACTORED=$BLKSIZE_UNFACTORED
            BLKSIZE="$(echo $BLKSIZE_UNFACTORED / $FACTOR | bc)"
            info BLKSIZE=$BLKSIZE
            if [ $BLKSIZE -le 60 ]
            then
                running info sudo parted -s $USBDEV mklabel msdos
                running info sudo parted -s $USBDEV mkpart primary 1MiB 551MiB
                running info sudo parted -s $USBDEV set 1 esp on
                running info sudo parted -s $USBDEV set 1 boot on
                running info sudo mkfs.fat -F32 ${USBDEV}1
                running info sudo parted -s $USBDEV mkpart primary 551MiB 100%
                running info sudo mkfs.ext4 ${USBDEV}2

                running info mkdir -p $WSPACEDIR/efi
                running info mkdir -p $WSPACEDIR/data

                running info sudo mount ${USBDEV}1 $WSPACEDIR/efi
                running info sudo mount ${USBDEV}2 $WSPACEDIR/data
                running info sudo mkdir -p $WSPACEDIR/data/boot/grub
                running info sudo touch $WSPACEDIR/data/boot/grub/grub.cfg
                running info sudo mkdir -p $WSPACEDIR/data/boot/iso
                running info sudo chown 1000:1000 $WSPACEDIR/data/boot/iso


                # Installing grub for efi
                sudo grub-install \
                     --target=x86_64-efi \
                     --recheck \
                     --removable \
                     --efi-directory="$WSPACEDIR/efi" \
                     --boot-directory="$WSPACEDIR/data/boot"

                if false
                then
                    # Installing legacy grub2
                    sudo grub2-install \
                         --target=i386-pc \
                         --recheck \
                         --boot-directory="$WSPACEDIR/data/boot" $$USBDEV
                fi

            else
                error $USBDEV size = $BLKSIZEis greater than 60
            fi
        fi
    else
        info $USBDEV not exists
    fi
}


##
function setup_finish()
{
    : info $SETUP_TMPDIR
}

function process_arg()
{
    warn=1
    error=1

    if ! eval set -- $(getopt -n $pgm -o "dehnvw" -- $*)
    then
        verbose Wrong command line.
    fi

    while [ $# -gt 0 ]
    do
        info option $1
        case $1 in
            (-n) noaction="";;
            (-d) debug=1;;
            (-v) verbose=1;;
            (-w) warn="";;
            (-e) error="";;
            (-h) help;
                 exit;;
            (--) shift; break;;
            (-*) error "$0: error - unrecognized option $1" 1>&2; help; exit 1;;
            (*)  break;;
        esac
        shift
    done

    CMDARGS=$*
}

function running()
{
    local  notifier=$1
    local _cmd=$2
    shift
    shift

    $notifier $_cmd "$@"
    if [ ! $noaction ]
    then
        $_cmd $*
    fi
}

function print()
{
    echo "$*"
}

function error()
{
    notify "Error $*"  >&2
    logger "Error $*"
    exit -1
}

function warn()
{
    if [ $warn ] ; then
        notify "Warning: $*" >&2
    fi
    logger "Warning: $*"
}

function debug()
{
    if [ $debug ] ; then
        notify "Debug: $*" >&2
    fi
    logger "Debug: $*"
}

function verbose()
{
    if [ $verbose ] ; then
        notify "Info: $*" >&2
    fi
    logger "Info: $*"
}

function info()
{
    notify "$*" >&2
    : logger "$*"
}

function notify()
{
    print "${pgm}:" "$*"

    if [ ! -t 1 ]
    then
        notify-send "${pgm}:" "$*"
    fi
}

function logger()
{
    #creating prolem
    command logger -p local1.notice -t ${pgm} -i - $USER : "$*"
}



#verbose=1

pgm="$(basename $0)"
main "$@"
exit
