#!/usr/bin/env zsh


processes=(firefox skype emacs)
# max_cpu=100
# max_mem=50
max_cpu=200
max_mem=80
WM=stumpwm

function main() {

    process_arg $@

    setup_dbus_vars

    foreach p ($processes) {
        if pgrep $p >&/dev/null
        then
            eval max_cpu_pid_val=$(command ps -o user,pid,pcpu,pmem --no-headers $(command pgrep $p) | awk '{ if ( $3 > '$max_cpu' ) printf "(%d %d)\n", $2, $3 }')
            eval max_mem_pid_val=$(command ps -o user,pid,pcpu,pmem --no-headers $(command pgrep $p) | awk '{ if ( $4 > '$max_mem' ) printf "(%d %d)\n", $2, $4 }')

            pid_max_cpu=${max_cpu_pid_val[1]}
            pid_max_mem=${max_mem_pid_val[1]}

            val_max_cpu=${max_cpu_pid_val[2]}
            val_max_mem=${max_mem_pid_val[2]}

            if [ "x$pid_max_mem" != "x" ] ; then
                comm_max_cpu=$(ps h -o comm $pid_max_cpu)

                warn "Going to kill $p ($pid_max_mem) mem usage $val_max_mem exceeds $max_mem"
                # kill $pid_max_mem

                if [ -e $disable_file ] ; then
                    notify "monitru is disabled: kill $pid_max_mem"
                else
                    kill $pid_max_mem
                fi

                sleep 2s
                if command ps $pid_max_mem >& /dev/null ; then
                    if [ ! -e $disable_file ] ; then
                        kill -9 $pid_max_mem
                    fi
                fi
                warn "Killed $p ($pid_max_mem) mem usage $val_max_mem exceeds $max_mem"
            fi

            if [ "x$pid_max_cpu" != "x" ] ; then
                comm_max_cpu=$(ps h -o comm $pid_max_cpu)
                # if command ps $pid_max_cpu >&/dev/null ; then
                #     verbose "$(command ps -o user,pid,pcpu,pmem --no-headers $(command pgrep $p))"
                # fi

                warn "Going to kill $p ($pid_max_cpu) cpu usage $val_max_cpu exceeds $max_cpu"
                if [ -e $disable_file ] ; then
                    notify "monitru is disabled: kill $pid_max_cpu"
                else
                    kill $pid_max_cpu
                fi

                sleep 2s
                if command ps $pid_max_cpu >& /dev/null ; then
                    if [ ! -e $disable_file ] ; then
                        kill -9 $pid_max_cpu
                    fi
                fi
                warn "Killed $p ($pid_max_cpu) cpu usage $val_max_cpu exceeds $max_cpu"
            fi
        fi
    }

    # sleep 21s
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
    local MACHINE_ID
    if [ -r /var/lib/dbus/machine-id ]
    then
        MACHINE_ID=/var/lib/dbus/machine-id
    elif [ -r /etc/machine-id ]
    then
        MACHINE_ID=/etc/machine-id
    fi

    source ~/.dbus/session-bus/$(< ${MACHINE_ID})-${_DISPLAYMAJOR}

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
