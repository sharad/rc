#!/usr/bin/env zsh
# -*- major-mode: sh; -*-

# script

# [[file:~/.repos/git/user/rc/binx/maintainance/tidydir.org::*script][script:1]]
function main()
{
    if [ ! -d ~/Sink ]
    then
        print directory '~/Sink' do not exists >&1
        exit
    fi
    mkdir -p ~/.tidydir
    dir=$1
    dirFile=~/.tidydir/$(echo $(readlink -m $dir) | tr -s / | tr / _)

    if [ -e $dirFile ]
    then
        dirFile_tmp=${dirFile}_tmp
        dirFile_extra=${dirFile}_extra
        command ls -1 $dir > $dirFile_tmp
        comm -31 $dirFile $dirFile_tmp > $dirFile_extra
        if [ "$(cat $dirFile_extra |wc -l)" -gt 0 ]
        then
            xargs -d '\n' -a $dirFile_extra sh -c 'echo moving "$0" to ~/Sink'
            xargs -d '\n' -a $dirFile_extra sh -c 'mv "$0" ~/Sink'
        fi
        rm -f $dirFile_tmp
        rm -f $dirFile_extra
    else
        command ls -1 $dir > $dirFile
    fi
}

main $@
# script:1 ends here
