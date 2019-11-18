#!/usr/bin/env zsh

WM=stumpwm


function main() {

    process_arg $@

    xsession_intrusive_run
}

function xsession_intrusive_run()
{
    if type session_intrusive_run >/dev/null 2>&1
    then
        session_intrusive_run
    fi

    for prog in mail-notification
    do
        if whence -p $prog >& /dev/null &&
               ! pgrep $prog >> /dev/null 2>&1
        then
            $prog &!
            # echo started $prog >&2
	          pgrep $prog  >> /dev/null 2>&1
            sleep 1s;
        else
            print Not starting $prog  >> /dev/null 2>&1
        fi
    done
}

function mount_private_dir()
{
    if [ -r ~/.ecryptfs/Private.mnt ] &&
           [ -d "$(cat ~/.ecryptfs/Private.mnt)" ] &&
           [ -x $HOME/bin/ecryptfs-mount-private ] &&
           ! mount | grep $(cat ~/.ecryptfs/Private.mnt) >/dev/null 2>&1
    then
        # if [  -e $PRIVATE_DIR ] && [ "x" != "x$DISPLAY" ] ; then
        #     [ -x $HOME/bin/ecryptfs-mount-private ] &&
        #         timeout 7 $HOME/bin/ecryptfs-mount-private >&/dev/null
        # fi
        timeout 7 $HOME/bin/ecryptfs-mount-private >&/dev/null
    fi
}

function enable_mail_sync()
{
    ~/bin/syncimap -r               # it only get secinfo from x keyring; enable
}

function get_security_info()
{
 echo
}

function session_intrusive_run()
{
    mount_private_dir
    enable_mail_sync
}

function process_arg() {
    warn=1
    error=1
    # ui=basic
    ui=quiet
    timeout=7m

    disable_file=~/.var/comm/disable/$pgm
    if ! set -- $(getopt -n $pgm -o hivwet: -- $@)
    then
        verbose Wrong command line.
    fi
    while [ $# -gt 0 ]
    do
        case $1 in
            (-t) eval timeout=$2; shift;;
            (-i)
                interactive=1
                # ttyui
                ui=blinkenlights;;
            (-v) verbose=1;;
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

    source ~/.dbus/session-bus/$(< /var/lib/dbus/machine-id)-${_DISPLAYMAJOR}


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
