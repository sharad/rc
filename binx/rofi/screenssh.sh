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

    check_host $_host

    echo "$TERMINAL" "$TERMINAL_OPTIONS" ${=_prefixcmd} screen -d -m -x $_session >> ${_SCREEN_SEL}/test
    coproc (
        # create if not exists
        if ${=_prefixcmd_test} screen -x "$_session" -ls | grep 'No Sockets' >/dev/null 2>&1
        then
            ${=_prefixcmd_test} screen -d -m -S $_session >/dev/null 2>&1
        fi

        # attach, true is for saving time
        if true || ! ${=_prefixcmd_test} screen -x "$_session" -ls | grep 'No Sockets' >/dev/null 2>&1
        then
            exec "$TERMINAL" "$TERMINAL_OPTIONS" ${=_prefixcmd} screen -d -m -x $_session 1>&- >/dev/null 2>&1
            # exec 1>&-
            exit
        else
            rofi -e "No session $_session for $)host exists."
        fi
    )
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
    echo -en "\x00prompt\x1fhosts\n"
    echo -en "\0message\x1f<hosts</b>\n"
    cat $_HOST_HISTORY
    exec 1>&-
    exit
}

function list_session()
{
    local _host="$1"
    local _SESSION_HISTORY=${_SCREEN_SEL}/session_${_host}
    local _SESSION_HISTORY_NEW=${_SCREEN_SEL}/session_${_host}_new
    local _SESSION_HISTORY_TMP=${_SCREEN_SEL}/session_${_host}_tmp
    local _prefixcmd_test=$(prefixcmd 0 ${_host})

    echo $_host >> $_HOST_HISTORY

    if [ ! -e $_SESSION_HISTORY ]
    then
        # mkdir -p $(dirname $_SESSION_HISTORY)
        echo default >> $_SESSION_HISTORY
    fi

    echo -en "\x00prompt\x1fChange prompt\n"
    echo -en "\0message\x1f<b>${_host}</b> sessions\n"

    echo -en "\0urgent\x1f0,2\n"
    echo -en "\0active\x1f1\n"
    echo -en "\0markup-rows\x1ftrue\n"
    # echo -en "\0message\x1fSpecial <b>bold</b>message\n"
    # echo -en "aap\0icon\x1ffolder\n"
    # echo -en "-------------\0nonselectable\x1ftrue\n"
    coproc (
        local _Host=$(echo "$_host" | cut -d@ -f2)
        if nc -z $_Host 22
        then
            if timeout -k 10 5 ${=_prefixcmd_test} screen -ls | egrep '^	' | cut -d'	' -f2 | cut -d. -f2 | sort -u $_SESSION_HISTORY_NEW >/dev/null 2>&1
            then
                comm -31 $_SESSION_HISTORY $_SESSION_HISTORY_NEW > $_SESSION_HISTORY_TMP
                cat $_SESSION_HISTORY_TMP >> $_SESSION_HISTORY
                sort -u $_SESSION_HISTORY -o $_SESSION_HISTORY
            fi
        fi
    )
    cat $_SESSION_HISTORY | xargs -r printf -- ${_host}" %s\n"
    exec 1>&-
    exit
}

function check_host()
{
    local _host="$1"
    local _prefixcmd=$(prefixcmd 1 $_host)
    local _prefixcmd_test=$(prefixcmd 0 $_host)

    if [ "${_host}" != "localhost" ]
    then
        echo $SSH_AUTH_SOCK  >> ${_SCREEN_SEL}/test
        if [ "x" = "x$SSH_AUTH_SOCK" ]
        then
            echo Error SSH_AUTH_SOCK not set >> ${_SCREEN_SEL}/test
            coproc rofi -e "Error SSH_AUTH_SOCK not set"
            exit
        elif [ "$_prefixcmd_test" ]
        then
            echo test $SSH_AUTH_SOCK >> ${_SCREEN_SEL}/test

            local _Host=$(echo "$_host" | cut -d@ -f2)

            echo ${TIMEOUT} -k 10 8 nc -z "$_Host" 22 >> ${_SCREEN_SEL}/test
            if ! ${TIMEOUT} -k 10 8 nc -z "$_Host" 22 >/dev/null 2>&1
            then
                coproc rofi -e "can not access $_host"
                coproc "${TERMINAL}" -hold "${TERMINAL_OPTIONS}" ${=_prefixcmd_test} ps >/dev/null 2>&1
                exec 1>&-
                exit
            fi
        fi
    fi
}

main ${=1}

