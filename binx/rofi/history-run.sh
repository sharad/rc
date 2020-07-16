#!/usr/bin/env bash



function main()
{
    local _histfile="$1"
    local _sep=''
    sed -e "s/${_sep}/${_sep}${_sep}/g" -e "s@^: [[:digit:]]\\+:0;@${_sep}@g" ${_histfile} | rofi -dmenu -sep ${_sep}
}



main "$@"

