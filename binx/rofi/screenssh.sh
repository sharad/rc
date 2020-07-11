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
    local _terminal="$1"
    local _host="$2"

    if [ "$_host" != "localhost" ]
    then
        if [ "$_terminal" -eq 1 ]
        then
            echo "ssh -t -X -o PubkeyAuthentication=yes -o VisualHostKey=no $_host"
        else
            echo "ssh -o PubkeyAuthentication=yes -o VisualHostKey=no $_host"
        fi
    fi
}

function run_screen()
{
    _host="$1"
    _session="$2"
    _prefixcmd=$(prefixcmd 1 $_host)
    _prefixcmd_test=$(prefixcmd 0 $_host)

    echo coproc xterm -e ${=_prefixcmd} screen -d -m -x $_session >> ${_SCREEN_SEL}/test
    if ${=_prefixcmd_test} screen -x "$_session" -ls | grep 'No Sockets' >/dev/null 2>&1
    then
        ${=_prefixcmd_test} screen -d -m -S $_session >/dev/null 2>&1
    fi

    # coproc xterm -hold -e ${=_prefixcmd} screen -d -m -x $_session
    # exec 1>&-
    # exit

    if ! ${=_prefixcmd_test} screen -x "$_session" -ls | grep 'No Sockets' >/dev/null 2>&1
    then
        coproc xterm -e ${=_prefixcmd} screen -d -m -x $_session
        exec 1>&-
        exit
    # else
    #     rofi -e 'can not start'
    fi
    exec 1>&-
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
    _prefixcmd=$(prefixcmd 0 ${_host})

    echo $_host >> $_HOST_HISTORY

    if [ ! -e $_SESSION_HISTORY ]
    then
        # mkdir -p $(dirname $_SESSION_HISTORY)
        echo default >> $_SESSION_HISTORY
    fi

    ${=_prefixcmd} screen -ls | egrep '^	' | cut -d'	' -f2 | cut -d. -f2 >> $_SESSION_HISTORY >/dev/null 2>&1

    sort -u $_SESSION_HISTORY -o $_SESSION_HISTORY
    echo -en "\x00prompt\x1fChange prompt\n"
    echo -en "\0message\x1f<b>$_host</b> sessions\n"

    echo -en "\0urgent\x1f0,2\n"
    echo -en "\0active\x1f1\n"
    echo -en "\0markup-rows\x1ftrue\n"
    # echo -en "\0message\x1fSpecial <b>bold</b>message\n"
    # echo -en "aap\0icon\x1ffolder\n"
    # echo -en "-------------\0nonselectable\x1ftrue\n"
    cat $_SESSION_HISTORY | xargs -r printf -- ${_host}" %s\n"
}


main ${=1}

