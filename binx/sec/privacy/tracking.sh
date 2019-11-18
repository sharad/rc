#!/usr/bin/env zsh


processes=(firefox skype)
max_cpu=100
max_mem=50
WM=stumpwm

function main() {

    process_arg $@



}


function enable_tracking() {

}

function disable_tracking() {

}

function process_arg() {

    warn=1
    error=1

    disable_file=~/.var/comm/disable/$pgm
    set -- $(getopt -n $pgm -o hdrsivwe -- $@)
    while [ $# -gt 0 ]
    do
        case $1 in
            (-i) interactive=1;;
            (-v) verbose=1;;
            (-s)
                if [ -f $disable_file ] ; then
                     notify $pgm is disabled;
                     exit -1;
                else
                    notify $pgm is enabled;
                    exit 0;
                fi
                ;;
            (-d)
                if [ -f $disable_file ] ; then
                     notify $pgm is already disabled;
                else
                 if mkdir -p $(dirname $disable_file); touch $disable_file ; then
                     sync
                     notify $pgm is disabled;
                 fi
                fi
                exit;;
            (-r)
                if [ -f $disable_file ] ; then
                    sync
                    rm -f $disable_file && notify $pgm is enabled;
                else
                    notify $pgm is already enabled;
                fi
                exit;;
            (-w) warn="";;
            (-e) error="";;
            (-h) help;
                 exit;;
            (--) shift; break;;
            (-*) echo "$0: error - unrecognized option $1" 1>&2; help; exit 1;;
            (*)  break;;
        esac
        shift
    done
}

function error() {
    notify "$*"
    logger "$*"
}

function warn() {
    if [ $warn ] ; then
        notify "$*"
    fi
    logger "$*"
}

function verbose() {
    if [ $verbose ] ; then
        notify "$*"
    fi
    logger "$*"
}

function notify() {
    if [ -t 1 ] ; then
        # echo -e "${pgm}:" "$*" >&2
        print "${pgm}:" "$*" >&2
    else
        notify-send "${pgm}:" "$*"
    fi
}

function logger() {
    #creating prolem
    command logger -p local1.notice -t ${pgm} -i - $USER : "$*"
}

pgm=$(basename $0)

main $@
