#!/usr/bin/env bash

# LOTUS_GUIX_NOPULL

function main()
{

    trap setup_finish EXIT SIGINT SIGTERM

    # calculate
    GNU_STORE_MINIMUM_AVAIL_MEGABYTES=300
    # make 21% of available space of /gnu/store
    GUIX_CLEANUP_MIN_SPACE_PERCENTAGE=21
    GNU_STORE_AVAIL_MEGABYTES="$(df -BM --output=avail  /gnu/store | sed -n -e 's/[^[:digit:]]//g' -e 2p)" # not used
    GNU_STORE_SIZE_GIGABYTES="$(df -BG --output=size  /gnu/store | sed -n -e 's/[^[:digit:]]//g' -e 2p)"
    GUIX_CLEANUP_MIN_SPACE="$(expr $GNU_STORE_SIZE_GIGABYTES '*' $GUIX_CLEANUP_MIN_SPACE_PERCENTAGE / 100)"
    # calculate


    DEFAULT_SYSTEM_ABONDONED_PKG_CLEANUP_MIN_SPACE=${GUIX_CLEANUP_MIN_SPACE}G
    DEFAULT_SYSTEM_ABONDONED_PKG_CLEANUP_MIN_TIME=30d
    DEFAULT_SYSTEM_GENERATION_CLEANUP_TIME=10m
    DEFAULT_USER_GENERATION_CLEANUP_TIME=96h

    running debug process_arg $@


    info printing default values
    info DEFAULT_SYSTEM_ABONDONED_PKG_CLEANUP_MIN_SPACE=$DEFAULT_SYSTEM_ABONDONED_PKG_CLEANUP_MIN_SPACE  -- 21 percentage of store size
    info DEFAULT_SYSTEM_ABONDONED_PKG_CLEANUP_MIN_TIME=$DEFAULT_SYSTEM_ABONDONED_PKG_CLEANUP_MIN_TIME
    info DEFAULT_SYSTEM_GENERATION_CLEANUP_TIME=$DEFAULT_SYSTEM_GENERATION_CLEANUP_TIME
    info DEFAULT_USER_GENERATION_CLEANUP_TIME=$DEFAULT_USER_GENERATION_CLEANUP_TIME
    info printed default values
    running info sleep 10s

    SYSTEM_ABONDONED_PKG_CLEANUP_MIN_SPACE=${SYSTEM_ABONDONED_PKG_CLEANUP_MIN_SPACE:-${DEFAULT_SYSTEM_ABONDONED_PKG_CLEANUP_MIN_SPACE}}
    SYSTEM_ABONDONED_PKG_CLEANUP_MIN_TIME=${SYSTEM_ABONDONED_PKG_CLEANUP_MIN_TIME:-30d}
    SYSTEM_GENERATION_CLEANUP_TIME=${SYSTEM_GENERATION_CLEANUP_TIME:-10m} # 10 months
    USER_GENERATION_CLEANUP_TIME=${USER_GENERATION_CLEANUP_TIME:-96h}

    echo
    info printing applied values
    info SYSTEM_ABONDONED_PKG_CLEANUP_MIN_SPACE=$SYSTEM_ABONDONED_PKG_CLEANUP_MIN_SPACE
    info SYSTEM_ABONDONED_PKG_CLEANUP_MIN_TIME=$SYSTEM_ABONDONED_PKG_CLEANUP_MIN_TIME
    info SYSTEM_GENERATION_CLEANUP_TIME=$SYSTEM_GENERATION_CLEANUP_TIME
    info USER_GENERATION_CLEANUP_TIME=$USER_GENERATION_CLEANUP_TIME
    info printed applied values
    echo
    running info sleep 10s

    if [ -f "$HOME/.setup/guix-config/per-user/$USER/meta/current" ]
    then
        LOCAL_GUIX_EXTRA_PROFILES=( $(cat "$HOME/.setup/guix-config/per-user/$USER/meta/current" | grep -v "01-simple" ) )
        # LOCAL_GUIX_EXTRA_PROFILES=( $(cat "$HOME/.setup/guix-config/per-user/$USER/meta/current" ) )
    else
        LOCAL_GUIX_EXTRA_PROFILES=("01-dev" "01-console" "01-x" "01-dynamic-hash" "90-heavy" "60-lengthy")
    fi
    export LOCAL_GUIX_EXTRA_PROFILES
    LOCAL_GUIX_EXTRA_PROFILE_CONTAINER_DIR="$HOME/.setup/guix-config/per-user/$USER/profiles"


    if [ -d "/run/current-system/profile" ]
    then


        if true
        then
            GUIX_TMPDIR="$(grep /srv/guix /etc/fstab | cut -f2)"
            if [ "x" != "x${GUIX_TMPDIR}" ]
            then

                running info sudo umount "${GUIX_TMPDIR}"
                sleep 1s

                if ! running info sudo mount "${GUIX_TMPDIR}"
                then
                    error Failed in mounting "${GUIX_TMPDIR}"
                    exit -1
                fi
            else
                error GUIX_TMPDIR not found
                exit -1
            fi
        fi

        if [ "x" != "x$LOTUS_GUIX_NOPULL" ] || [ "$GNU_STORE_AVAIL_MEGABYTES" -lt "$GNU_STORE_MINIMUM_AVAIL_MEGABYTES" ] || running info guix pull
        then
            running info guix pull --news

            running info df -hx tmpfs -x devtmpfs
            running info sleep 10s

            if true
            then

                running info guix package  --delete-generations=${USER_GENERATION_CLEANUP_TIME} # for "01-simple"


                for profile in "${LOCAL_GUIX_EXTRA_PROFILES[@]}"
                do
                    profile_container_path="${LOCAL_GUIX_EXTRA_PROFILE_CONTAINER_DIR}/${profile}"
                    manifest_path="${profile_container_path}/manifest.scm"
                    profile_path="${profile_container_path}/profiles.d/profile"
                    broken_path="${profile_container_path}/broken"

                    mkdir -p "${broken_path}"
                    find "${profile_container_path}/profiles.d" -xtype l -exec mv {} "${broken_path}" \;

                    if [ -f "${manifest_path}" -a -f "${profile_path}/etc/profile" ]
                    then
                        running info guix package -p "${profile_path}" --delete-generations=${USER_GENERATION_CLEANUP_TIME}
                    else
                        warn file "${profile_path}"/etc/profile not exist, for "${profile_path}"
                    fi
                    unset profile_path
                    unset profile
                done

                running info sudo guix system delete-generations ${SYSTEM_GENERATION_CLEANUP_TIME}
                running info guix gc -d ${SYSTEM_ABONDONED_PKG_CLEANUP_MIN_TIME} -C  ${SYSTEM_ABONDONED_PKG_CLEANUP_MIN_SPACE}

            fi
            running info df -hx tmpfs -x devtmpfs
        fi                      # if [ "x" != "x$LOTUS_GUIX_NOPULL" ] || [ "$GNU_STORE_AVAIL_MEGABYTES" -lt "$GNU_STORE_MINIMUM_AVAIL_MEGABYTES" ] || running info guix pull

        if true
        then
            GUIX_TMPDIR="$(grep /srv/guix /etc/fstab | cut -f2)"
            if [ "x" != "x${GUIX_TMPDIR}" ]
            then

                sudo umount "${GUIX_TMPDIR}"

                if ! running info sudo umount "${GUIX_TMPDIR}"
                then
                    error Failed in unmounting "${GUIX_TMPDIR}"
                    exit -1
                fi
            else
                error GUIX_TMPDIR not found
                exit -1
            fi
        fi
    fi

}






function process_arg()
{
    warn=1
    error=1

    if ! set -- $(getopt -n $pgm -o "s:a:g:u:nehvdw" -- $@)
    then
        verbose Wrong command line.
    fi

    # info SYSTEM_ABONDONED_PKG_CLEANUP_MIN_SPACE=$SYSTEM_ABONDONED_PKG_CLEANUP_MIN_SPACE
    # info SYSTEM_ABONDONED_PKG_CLEANUP_MIN_TIME=$SYSTEM_ABONDONED_PKG_CLEANUP_MIN_TIME
    # info SYSTEM_GENERATION_CLEANUP_TIME=$SYSTEM_GENERATION_CLEANUP_TIME
    # info USER_GENERATION_CLEANUP_TIME=$USER_GENERATION_CLEANUP_TIME

    while [ $# -gt 0 ]
    do
        echo option $1
        case $1 in
            (-s) eval SYSTEM_ABONDONED_PKG_CLEANUP_MIN_SPACE=$2; shift;;
            (-a) eval SYSTEM_ABONDONED_PKG_CLEANUP_MIN_TIME=$2; shift;;
            (-g) eval SYSTEM_GENERATION_CLEANUP_TIME=$2; shift;;
            (-u) eval USER_GENERATION_CLEANUP_TIME=$2; shift;;
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
}

function help()
{
    cat <<-EOF
    -s : SYSTEM_ABONDONED_PKG_CLEANUP_MIN_SPACE=$SYSTEM_ABONDONED_PKG_CLEANUP_MIN_SPACE
    -a : SYSTEM_ABONDONED_PKG_CLEANUP_MIN_TIME=$SYSTEM_ABONDONED_PKG_CLEANUP_MIN_TIME
    -g : SYSTEM_GENERATION_CLEANUP_TIME=$SYSTEM_GENERATION_CLEANUP_TIME
    -u : USER_GENERATION_CLEANUP_TIME=$USER_GENERATION_CLEANUP_TIME
    -n : no action
    -d : debug
    -v : verbose
    -w : warn
    -e : error
    -h : this help
EOF
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
        $_cmd "$@"
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

function setup_finish()
{
    echo
}

#verbose=1

pgm="$(basename $0)"
main "$@"
exit
