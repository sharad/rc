#!/usr/bin/env zsh
#  -*- mode: sh; -*-
# git-sync
#
# sychronize tracking repositories
#
# 2012 by Simon Thum
# Licensed as: CC0
#
# This scrips intends to sync via git near-automatically
# in "tracking" repositories where a nice history is not
# crucial as having one.
#
# Unlike the myriad of scripts to do just that already available,
# it follows the KISS principle: It is small, requires nothing but
# git and bash, but does not even try to shield you from git.
#
# Mode sync (default)
#
# Sync will likely get from you from a dull normal git repo with trivial
# changes to an updated dull normal git repo equal to origin. No more,
# no less. The intent is to do everything that's needed to sync
# automatically, and resort to manual intervention as soon
# as something non-trivial occurs. It is designed to be safe
# in that it will likely refuse to do anything not known to
# be safe.
#
# Mode check
#
# Check only performs the basic checks sync checks for to
# make sure the repository is in a state sufficiently "normal"
# to continue syncing, i.e. committing changes, pull etc. without
# losing any data. When check returns 0, sync can start immediately.
# This does not, however, indicate that syncing is at all likely to
# succeed.

function main()
{
    process_arg $@
    CURR_BRANCH=$(git rev-parse --abbrev-ref HEAD)
    if [ "HEAD" != "$CURR_BRANCH" ]
    then
        BRANCH_REMOTE=$(git config branch.${CURR_BRANCH}.remote )
        BRANCH_MERGE=$(git config branch.${CURR_BRANCH}.merge )
        if [ 0 = $? ] && [ "x" != "x$BRANCH_MERGE" ]
        then
            if [ $stash ]
            then
                if git diff --exit-code --quiet
                then
                    STASH_FOR_REBASE=1
                    running git stash save "stating to rebase $CURR_BRANCH"
                fi
            fi


            if [ $recursive ]
            then
                if [ "." != "$BRANCH_REMOTE" ]
                then
                    running git checkout $BRANCH_MERGE
                    running $0
                    running git checkout $CURR_BRANCH
                fi
            fi # if [ $force ]
            running git pull -v --rebase $BRANCH_REMOTE $BRANCH_MERGE
        else
            echo Not able to find base branch, exiting. >&2
        fi
    else
        echo Not able to find branch name, exiting. >&2
    fi

    PRESENT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
    echo current branch is $PRESENT_BRANCH

    if [ $STASH_FOR_REBASE ]
    then
        running git stash pop
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
