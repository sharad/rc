#!/usr/bin/env zsh
#  -*- mode: sh; -*-

WM=stumpwm

MAILDIR=~/.maildir


function main() {

    process_arg $@

    # gnome-keyring-attach

    if [ -L ${MAILDIR} -a -d ${MAILDIR} ]
    then
        if [ -e $disable_file ] ; then
            notify "Syncimap is disabled"
            exit 0
        fi

        if [ ! -e ~/.offlineimaprc ] ; then
            verbose no ~/.offlineimaprc do not exists.
            exit -1
        else
            # pkill offlineimap
            # if nm-tool | egrep -q 'DNS:[[:space:]]+[1-9]' ||
            #   nm-tool | egrep -q 'State:[[:space:]]+connected' ||
            #   true ; then
	          if true             # test if  net connection is up
	          then
                if ! pgrep offlineimap 2>&1 > /dev/null ; then
                    foreach acc ( $(echo ${account:-$OFFLINEIMAPACCOUNT}  | tr , ' ' ) ) {

                        verbose timeout -s KILL $timeout offlineimap -1 -u $ui -a $acc
                        timeout -s KILL $timeout offlineimap -1 -u $ui -a $acc

                    }
                else
                    notify already offline map running with pid $(pgrep offlineimap).
                    exit 0;
                fi
            else
                verbose no network connectivity.
                exit 0;
            fi
        fi
    else                        # if [ -L ${MAILDIR} -a -d ${MAILDIR} ]
        notify ${MAILDIR} do not exits, exiting...
        exit -1;
    fi                          # if [ -L ${MAILDIR} -a -d ${MAILDIR} ]
}

function process_arg() {
    warn=1
    error=1
    # ui=basic
    ui=quiet
    timeout=7m

    disable_file=~/.var/comm/disable/$pgm
    if ! set -- $(getopt -n $pgm -o hdrsiu:vwea:t: -- $@)
    then
        verbose Wrong command line.
    fi
    while [ $# -gt 0 ]
    do
        case $1 in
            (-a) eval account=$2; shift;;
            (-t) eval timeout=$2; shift;;
            (-i)
                interactive=1
                # ttyui
                ui=blinkenlights;;
            (-u) eval ui=$2; shift;;
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


function gnome-keyring-attach() {
   # local -a vars
    local vars

    vars=( \
        DISPLAY \
        DBUS_SESSION_BUS_ADDRESS \
        SSH_AUTH_SOCK \
        SSH_AGENT_PID \
        XDG_SESSION_COOKIE \
    )

    if pgrep ${WM} 2>&1 > /dev/null ; then
        local pid=$(command ps -C ${WM} -o pid --no-heading | tr -d ' ')
        eval "unset ${vars[@]}; $(printf "export %s;" $(sed 's/\x00/\n/g' /proc/${pid//[^0-9]/}/environ | grep $(printf -- "-e ^%s= " "${vars[@]}")) )"

    else
        exit 1;
    fi

    local _DISPLAYMAJOR=$(echo ${DISPLAY} | cut -f2 -d: | cut -d. -f1)
    local MACHINE_ID
    if [ -r /var/lib/dbus/machine-id ]
    then
        MACHINE_ID=/var/lib/dbus/machine-id
    elif [ -r /etc/machine-id ]
    then
        MACHINE_ID=/etc/machine-id
    fi

    source ~/.dbus/session-bus/$(< ${MACHINE_ID})-${_DISPLAYMAJOR}


    if ! timeout -s KILL 2 ~/bin/get-imap-pass localhost 2>&1 > /dev/null; then
        warn "Keyring is not responding. Please check error with get-imap-pass $DBUS_SESSION_BUS_ADDRESS"
        if false && pkill gnome-keyring && get-imap-pass ; then
            verbose "Restarted keyring"
        fi
        if sleep 5s; timeout -s KILL 10 ~/bin/get-imap-pass 2>&1 > /dev/null ; then
            verbose "Restarting keyring after 10 seconds."
        else
# error "Need to restart keyring"
# if pkill gnome-keyring ; then
#     warn "Restarted keyring successfully."
# else
#     error "Failed to restart keyring."
# fi
	    exit -1;
        fi
    fi
}

pgm=$(basename $0)

main $@
