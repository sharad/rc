#!/bin/zsh

function main() {


    process_arg $@
    if [ ! -e ~/.offlineimaprc ] ; then
        verbose no ~/.offlineimaprc do not exists.
        exit -1
    else
        # pkill offlineimap
        if nm-tool | egrep -q 'DNS:[[:space:]]+[1-9]' ||
           nm-tool | egrep -q 'State:[[:space:]]+connected'; then
            if ! pgrep offlineimap >& /dev/null; then
                if [  $interactive  ] ; then
                    offlineimap -a ${account:-$OFFLINEIMAPACCOUNT}
                else
                    # wait till 3 min then send SIGINT.
                                                                                                              # put it into some log.
                    # xwarn="$( timeout -s INT 360 offlineimap -u Noninteractive.Quiet -a ${account:-$OFFLINEIMAPACCOUNT} |& egrep -i 'WARNING|Error' )" &&
                    # warn "Some problem with OfflineImap\nPlease check as soon as possible.\n${xwarn}"

                    xwarn="$( timeout -s INT 360 offlineimap -u quiet -a ${account:-$OFFLINEIMAPACCOUNT} |& egrep -i 'WARNING|Error' )" &&
                    warn "Some problem with OfflineImap\nPlease check as soon as possible.\n${xwarn}"
                fi
            else
                verbose already offline map running with pid $(pgrep offlineimap).
            fi
        else
            verbose no network connectivity.
        fi
    fi
}

function process_arg() {
    set -- $(getopt -n $pgm -o ivwea: -- $@)
    while [ $# -gt 0 ]
    do
        case $1 in
            (-a) eval account=$2; shift;;
            (-i) interactive=1;;
            (-v) verbose=1;;
            (-w) warn=1;;
            (-e) error=1;;
            (--) shift; break;;
            (-*) echo "$0: error - unrecognized option $1" 1>&2; exit 1;;
            (*)  break;;
        esac
        shift
    done
}

function warn() {
    if [ $warn ] ; then
        notify $@
    fi
    logger $@
}

function verbose() {
    if [ $verbose ] ; then
        notify $@
    fi
    logger $@
}

function notify() {
    if [ -t 1 ] ; then
        print ${pgm}: $@
    else
        notify-send ${pgm}: $@
    fi
}

function logger() {
    logger -p local1.notice -t ${pgm} -i - $USER : $@
}

pgm=$(basename $0)

main $@
