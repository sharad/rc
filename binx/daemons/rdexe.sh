#!/usr/bin/env bash

# Starting listener on some port
# we will run it as deamon and we will send commands to it.
#
DAEMON_RDEXE_DIR=~/.local/var/rdexe


function main()
{
    subcmd=$1
    shift 1
    $subcmd "$@"
}

function daemon()
{
    mkdir -p $DAEMON_RDEXE_DIR
    mkdir -p $DAEMON_RDEXE_DIR/lscreen
    mkdir -p $(dirname "${FILE}")

    while true
    do
        sleep 2s;
        run_screens_local
        sleep 1s
        run_screens_remote
    done
}

function run_nc_shell()
{
    IP=$(hostname --ip-address)
    PORT=1024
    FILE=~/tmp/volatile/daemon/backpipe
    count=0
    while [ -a $FILE ]
    do #If file exis I assume that it used by other program
        FILE=$FILE.$count
        count=$(($count + 1))
    done

    # Now we know that such file do not exist,
    # U can write down in deamon it self the remove for those files
    # or in different part of program
    mknod $FILE p
    netcat -c -l -p $PORT < $FILE | bash > $FILE
    rm $FILE
}

function run_screens_local()
{
    for lscn in $(command ls -1 $DAEMON_RDEXE_DIR/lscreens)
    do
        run_screen_local $DAEMON_RDEXE_DIR/lscreens/$lscn &
    done
}

function run_screen_local()
{
    file=$1
    if read -p "file is avaialble to should i run: "
    then
        # get remote server user file
        # run
        rm -f $file
    fi
}


function run_screens_remote()
{
    for lscn in $(command ls -1 $DAEMON_RDEXE_DIR/lscreens)
    do
        run_screen_remote $DAEMON_RDEXE_DIR/rscreens/$lscn &
        rm -f $DAEMON_RDEXE_DIR/rscreens/$lscn
    done
}

function run_screen_remote()
{
    file=$1
    # get remote server user file
    if timeout -k 8 4 scp user@server:file $TMP/file
    then
        if read -p "file is avaialble to should i run: "
        then
            run_screen_local $TMP/file
            timeout -k 8 4 ssh user@server rm -f file
        fi
    fi
}

function create_screen()
{
    echo
}


main "$@"




