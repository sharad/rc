#!/usr/bin/env bash

sentinel="##@@##"
if [[ $(id -u) != "0" ]]; then
    absScriptPath="$( cd "$(dirname "$0")" ; pwd -P )/$(basename "$0")"
    pkexec "$absScriptPath" "$sentinel" "$DISPLAY" "$XAUTHORITY" "$DBUS_SESSION_BUS_ADDRESS" "$@"
    exit 0
fi
# root here
if [[ "$1" == "$sentinel" ]]; then
    shift
    export DISPLAY="$1"
    shift
    export XAUTHORITY="$1"
    shift
    export DBUS_SESSION_BUS_ADDRESS="$1"
    shift
fi

$@
