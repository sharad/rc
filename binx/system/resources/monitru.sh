#!/bin/zsh


processes=(firefox)
max_cpu=100
max_mem=50
WM=stumpwm

function main() {

    process_arg $@

    setup_dbus_vars

    if [ -e $disable_file ] ; then
        notify "Syncimap is disabled"
        exit 0
    fi

    foreach p ($processes)
    {
        pid_max_cpu=$(command ps -o user,pid,pcpu,pmem --no-headers $(command pgrep $p) | awk '{ if ( $3 > '$max_cpu' ) print $2}')
        pid_max_mem=$(command ps -o user,pid,pcpu,pmem --no-headers $(command pgrep $p) | awk '{ if ( $4 > '$max_mem' ) print $2}')

        verbose "$(command ps -o user,pid,pcpu,pmem --no-headers $(command pgrep $p))"

        if [ "x$pid_max_mem" != "x" ] ; then
            comm_max_cpu=$(ps h -o comm $pid_max_cpu)
            # echo $pid_max_cpu
            # echo $pid_max_mem

            warn "Going to kill $p mem usage $pid_max_mem exceeds $max_mem"
            kill $pid_max_mem
            sleep 2s
            if command ps $pid_max_mem >&/dev/null ; then
                kill -9 $pid_max_mem
            fi
            warn "Killed $p mem usage $pid_max_mem exceeds $max_mem"
        fi

        if [ "x$pid_max_cpu" != "x" ] ; then
            comm_max_cpu=$(ps h -o comm $pid_max_cpu)
            # echo $pid_max_cpu
            # echo $pid_max_cpu

            warn "Going to kill $p cpu usage $pid_max_cpu exceeds $max_cpu"
            kill $pid_max_cpu
            sleep 2s
            if command ps $pid_max_cpu >&/dev/null ; then
                kill -9 $pid_max_cpu
            fi
            warn "Killed $p cpu usage $pid_max_cpu exceeds $max_cpu"
        fi

        # if [ ${processes[(r)comm_max_mem]} = $comm_max_mem ] ; then
        #     kill $pid_max_mem
        #     sleep 2s
        #     kill -9 $pid_max_mem
        # fi
    }

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

function setup_dbus_vars() {
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

    source ~/.dbus/session-bus/$(< /var/lib/dbus/machine-id)-${_DISPLAYMAJOR}

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
