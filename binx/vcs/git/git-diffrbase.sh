#!/usr/bin/env zsh
#  -*- mode: sh; -*-
# git-sync
#
# sychronize tracking repositories
#
# 2018 by Sharad
# Licensed as: CC0
#

function main()
{
    process_arg $@
    CURR_BRANCH=$(git rev-parse --abbrev-ref HEAD)
    if [ "HEAD" != "$CURR_BRANCH" ]
    then
        BRANCH_REMOTE=$(git config branch.${CURR_BRANCH}.remote )
        BRANCH_MERGE=$(git config branch.${CURR_BRANCH}.merge )
        if [ "." != "$BRANCH_REMOTE" ]
        then
            echo Remote base diff not possible, as current branch $CURR_BRANCH created from remote branch $BRANCH_REMOTE/$BRANCH_MERGE >&2
            exit -1
        fi

        while [ "." = "$BRANCH_REMOTE" ]
        do
            BRANCH_REMOTE=$(git config branch.${BRANCH_MERGE}.remote )
            BRANCH_MERGE=$(git config branch.${BRANCH_MERGE}.merge )
        done

        git diff $BRANCH_MERGE --
    else
        echo Not able to find branch name, exiting. >&2
    fi
}

function process_arg() {
    warn=1
    error=1

    if ! set -- $(getopt -n $pgm -o hdrsiu:vwea:t: -- $@)
    then
        verbose Wrong command line.
    fi
    while [ $# -gt 0 ]
    do
        case $1 in
            (-r) recursive=1;;
            (-n) stash="";;
            (-n) noaction="";;
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

function running()
{
    echo $@
    if [ ! $noaction ]
    then
        $@
    fi
}

main
