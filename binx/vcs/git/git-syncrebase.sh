#!/usr/bin/env bash
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
    # echo main
    # echo ${args[@]}
    # echo 0 ${args[0]}
    # echo 1 ${args[1]}
    # echo ${args[*]}
    # exit 

    CURR_BRANCH=$(git rev-parse --abbrev-ref HEAD)
    if [ "HEAD" != "$CURR_BRANCH" ]
    then
        BRANCH_REMOTE=$(git config branch.${CURR_BRANCH}.remote )
        BRANCH_MERGE_PATH=$(git config branch.${CURR_BRANCH}.merge )

        if [ "." != "${BRANCH_REMOTE}" -a -n "${args[1]}" ]
        then
            echo changeing remote from "${BRANCH_REMOTE}" to "${args[1]}"
            BRANCH_REMOTE="${args[1]}"
        fi

        if [ "$BRANCH_MERGE_PATH" ]
        then
            BRANCH_MERGE=$(basename ${BRANCH_MERGE_PATH})
        fi

        echo ${CURR_BRANCH}: "remote: ${BRANCH_REMOTE}"
        echo ${CURR_BRANCH}: "merge:  ${BRANCH_MERGE}"
        echo ${CURR_BRANCH}: "${BRANCH_REMOTE}/${BRANCH_MERGE}"

        # if [ 0 = $? ] && [ "x" != "x$BRANCH_MERGE" ]
        if [ "x" != "x$BRANCH_MERGE" ]
        then
            if [ $stash ]
            then
                if ! git diff --exit-code --quiet
                then
                    STASH_FOR_REBASE=1
                    running git stash save "stating to rebase $CURR_BRANCH"
                fi
            fi


            if [ $recursive ]
            then
                if [ "." = "$BRANCH_REMOTE" ]
                then
                    running git checkout $BRANCH_MERGE
                    running $pgm $@
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

    if ! set -- $(getopt -n $pgm -o rnsehvw -- $@)
    then
        verbose Wrong command line.
    fi

    while [ $# -gt 0 ]
    do
        case "$1" in
            (-r) recursive=1;;
            (-s) stash=1;;
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
        # echo -- shift "$1"
        shift
        # echo -- $*
    done
    
    eval args=( $0 "$@" )
}

function running()
{
    echo running $@
    if [ ! $noaction ]
    then
        $@
    fi
}

pgm=$0

main $@
