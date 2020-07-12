#!/usr/bin/env bash



function main()
{
    local _histfile="$1"
    sed -e 's/^A/^A^A/g' -e 's@^: [[:digit:]]\+:0;@^A@g' ${_histfile}
}



main "$@"

