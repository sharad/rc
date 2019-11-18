#!/usr/bin/env zsh

# vars
VAR=VALUE


function main() {

    process_arg $@
}

function process_arg() {
    warn=1
    error=1
    # ui=basic
    ui=quiet
    timeout=7m

    disable_file=~/.var/comm/disable/$pgm
    if ! set -- $(getopt -n $pgm -o "h" -- $@)
    then
        verbose Wrong command line.
    fi
    while [ $# -gt 0 ]
    do
        case $1 in
            # (-a) eval account=$2; shift;;
            # (-t) eval timeout=$2; shift;;
            # (-v) verbose=1;;
            (-h) help;
                 exit;;
            (--) shift; break;;
            (-*) echo "$0: error - unrecognized option $1" 1>&2; help; exit 1;;
            (*)  break;;
        esac
        shift
    done
}

function help() {
    cat <<'EOF'
            -a: eval account=$2; shift;;
            -i: interactive=1;;
            -u: user interface=$2; shift;;
            -v: verbose=1;;
            -d: touch $disable_file;;
            -r: rm -f $disable_file;;
            -w: warn=1;;
            -e: error=1;;
            -h: help;;
EOF
}

function error() {
    notify "$*"  >&2
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
        print "${pgm}:" "$*" >&2
        notify-send "${pgm}:" "$*"
    fi
}

function logger() {
    #creating prolem
    command logger -p local1.notice -t ${pgm} -i - $USER : "$*"
}



pgm=$(basename $0)

main $@
