#!/usr/bin/env bash


function main()
{

    trap setup_finish EXIT SIGINT SIGTERM

    running debug process_arg $@
    USER_GENERATION_CLEANUP_TIME=${USER_GENERATION_CLEANUP_TIME:-96h}
    SYSTEM_GENERATION_CLEANUP_TIME=${SYSTEM_GENERATION_CLEANUP_TIME:-10m} # 10 months
    SYSTEM_ABONDONED_PKG_CLEANUP_MIN_TIME=${SYSTEM_ABONDONED_PKG_CLEANUP_MIN_TIME:-30d}
    SYSTEM_ABONDONED_PKG_CLEANUP_MIN_SPACE=${SYSTEM_ABONDONED_PKG_CLEANUP_MIN_SPACE:-3G}

    if [ -f "$HOME/.setup/guix-config/per-user/$USER/meta/current" ]
    then
        LOCAL_GUIX_EXTRA_PROFILES=( $(cat "$HOME/.setup/guix-config/per-user/$USER/meta/current" ) )
    else
        LOCAL_GUIX_EXTRA_PROFILES=("01-dev" "01-console" "01-x" "01-dynamic-hash" "90-heavy" "60-lengthy")
    fi
    export LOCAL_GUIX_EXTRA_PROFILES
    LOCAL_GUIX_EXTRA_PROFILE_CONTAINER_DIR="$HOME/.setup/guix-config/per-user/$USER/profiles"


    if [ -d "/run/current-system/profile" ]
    then
        if running info guix pull
        then
            running info guix pull --news

            running info df -hx tmpfs -x devtmpfs
            running info sleep 10s

            if true
            then

                running info guix package  --delete-generations=${USER_GENERATION_CLEANUP_TIME}
                for profile in "${LOCAL_GUIX_EXTRA_PROFILES[@]}"
                do
                    manifest_path="${LOCAL_GUIX_EXTRA_PROFILE_CONTAINER_DIR}/${profile}/manifest.scm"
                    profile_path="${LOCAL_GUIX_EXTRA_PROFILE_CONTAINER_DIR}/${profile}/profiles.d/profile"
                    if [ -f "${manifest_path}" -a -f "${profile_path}"/etc/profile ]
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


        fi
    fi

}



































function process_arg()
{
    warn=1
    error=1

    if ! set -- $(getopt -n $pgm -o "rnsehvdw" -- $@)
    then
        verbose Wrong command line.
    fi

    while [ $# -gt 0 ]
    do
        echo option $1
        case $1 in
            (-r) recursive=1;;
            (-s) stash=1;;
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
