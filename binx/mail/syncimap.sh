#!/bin/bash

WM=stumpwm


function main() {



    process_arg $@

    gnome-keyring-attach

    if [ -e $disable_file ] ; then
        warn "Syncimap is disabled"
        exit 0
    fi

    if [ ! -e ~/.offlineimaprc ] ; then
        verbose no ~/.offlineimaprc do not exists.
        exit -1
    else
        # pkill offlineimap
        if nm-tool | egrep -q 'DNS:[[:space:]]+[1-9]' ||
           nm-tool | egrep -q 'State:[[:space:]]+connected'; then
            if ! pgrep offlineimap ; then
                echo y
                if [  $interactive  ] ; then
                    offlineimap -a ${account:-$OFFLINEIMAPACCOUNT}
                else
                    timeout -s KILL 70 offlineimap -1 -u quiet -a ${account:-$OFFLINEIMAPACCOUNT}
                fi
            else
                echo n
                exit 0;
                verbose already offline map running with pid $(pgrep offlineimap).
            fi
        else
            verbose no network connectivity.
            exit 0;
        fi
    fi
}

function process_arg() {

    disable_file=~/.var/comm/disable/$pgm
    set -- $(getopt -n $pgm -o hdrivwea: -- $@)
    while [ $# -gt 0 ]
    do
        case $1 in
            (-a) eval account=$2; shift;;
            (-i) interactive=1;;
            (-v) verbose=1;;
            (-d)
                 if mkdir -p $(dirname $disable_file); touch $disable_file ; then
                     echo $pgm disabled;
                 fi
                 exit;;
            (-r) rm -f $disable_file;
                 echo $pgm enabled;
                 exit;;
            (-w) warn=1;;
            (-e) error=1;;
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
            -v: verbose=1;;
            -d: touch $disable_file;;
            -r: rm -f $disable_file;;
            -w: warn=1;;
            -e: error=1;;
            -h: help;;
EOF

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


function gnome-keyring-attach() {
    local -a vars=( \
        DBUS_SESSION_BUS_ADDRESS \
        SSH_AUTH_SOCK \
        SSH_AGENT_PID \
        XDG_SESSION_COOKIE \
    )
    if pgrep ${WM} ; then
        local pid=$(ps -C ${WM} -o pid --no-heading)
        eval "unset ${vars[@]}; $(printf "export %s;" $(sed 's/\x00/\n/g' /proc/${pid//[^0-9]/}/environ | grep $(printf -- "-e ^%s= " "${vars[@]}")) )"

    else
        exit 1;
    fi

    if        ! timeout -s KILL 4 ~/bin/get-imap-pass ; then
	exit 1;
	fi
}

pgm=$(basename $0)

main $@

