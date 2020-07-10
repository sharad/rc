#!/usr/bin/env zsh

_SCREEN_SEL=~/.local/var/cache/rofi

_HOST_HISTORY=${_SCREEN_SEL}/hosts


function main()
{
    local _host
    local _session


    if [ $# -eq 0 ]
    then
        list_hosts
    elif [ $# -eq 1 ]
    then
        _host="$1"
        list_session $_host
    else
        _host="$1"
        _session="$2"
        run_screen "$_host" "$_session"
    fi
}



function prefixcmd()
{
    local _host="$1"

    if [ "$_host" != "localhost" ]
    then
        echo "ssh -X  -o VisualHostKey=no $_host"
    fi
}

function run_screen()
{
    _host="$1"
    _session="$2"
    _prefixcmd=$(prefixcmd $_host)

    echo xterm -e ${=_prefixcmd} screen -d -m -x $_session >> ${_SCREEN_SEL}/test
    exec xterm -e ${=_prefixcmd} screen -d -m -x $_session
    exit
}

function list_hosts()
{
    if [ ! -e $_HOST_HISTORY ]
    then
        mkdir -p $(dirname $_HOST_HISTORY)
        echo localhost >> $_HOST_HISTORY
    fi
    sort -u $_HOST_HISTORY -o $_HOST_HISTORY
    cat $_HOST_HISTORY
}

function list_session()
{
    local _host="$1"
    local _SESSION_HISTORY=${_SCREEN_SEL}/session_${_host}
    _prefixcmd=$(prefixcmd $_host)

    if [ ! -e $_SESSION_HISTORY ]
    then
        # mkdir -p $(dirname $_SESSION_HISTORY)
        echo default >> $_SESSION_HISTORY
    fi


    ${=prefixcmd} screen -ls | egrep '^	' | cut -d'	' -f2 | cut -d. -f2  >> $_SESSION_HISTORY
    sort -u $_SESSION_HISTORY -o $_SESSION_HISTORY
    cat $_SESSION_HISTORY | xargs -r printf -- ${_host}" %s\n"
}


main ${=1}

