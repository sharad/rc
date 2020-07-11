#!/usr/bin/env zsh

_SCREEN_SEL=~/.local/var/cache/rofi
_HOST_HISTORY=${_SCREEN_SEL}/hosts

TIMEOUT=timeout
TERMINAL=xterm
TERMINAL_OPTIONS="-e"


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
        check_host $_host
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

    echo coproc "$TERMINAL" "$TERMINAL_OPTIONS" ${=_prefixcmd} screen -d -m -x $_session >> ${_SCREEN_SEL}/test
    if ${=_prefixcmd_test} screen -x "$_session" -ls | grep 'No Sockets' >/dev/null 2>&1
    then
        ${=_prefixcmd_test} screen -d -m -S $_session >/dev/null 2>&1
    fi

    if ! ${=_prefixcmd_test} screen -x "$_session" -ls | grep 'No Sockets' >/dev/null 2>&1
    then
        coproc "$TERMINAL" "$TERMINAL_OPTIONS" ${=_prefixcmd} screen -d -m -x $_session >/dev/null 2>&1
        exec 1>&-
        exit
    # else
    #     echo RofiErrorCode
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
    local _prefixcmd_test=$(prefixcmd 0 ${_host})

    echo $_host >> $_HOST_HISTORY

    if [ ! -e $_SESSION_HISTORY ]
    then
        # mkdir -p $(dirname $_SESSION_HISTORY)
        echo default >> $_SESSION_HISTORY
    fi

    ${=_prefixcmd_test} screen -ls | egrep '^	' | cut -d'	' -f2 | cut -d. -f2 >> $_SESSION_HISTORY >/dev/null 2>&1

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

function check_host()
{
    local _host="$1"
    local _prefixcmd=$(prefixcmd 1 $_host)
    local _prefixcmd_test=$(prefixcmd 0 $_host)

    if [ "${_host}" != "localhost" ]
    then
        if [ "x" = "x$SSH_AUTH_SOCK" ]
        then
            echo Error SSH_AUTH_SOCK not set
            exit
        elif [ "$_prefixcmd_test" ]
        then
            if ! ${TIMEOUT} -k 10 8 ${=_prefixcmd_test} ps >/dev/null 2>&1
            then
                coproc "${TERMINAL}" -hold "${TERMINAL_OPTIONS}" ${=_prefixcmd_test} ps >/dev/null 2>&1
                exec 1>&-
                exit
            fi
        fi
    fi
}

main ${=1}

