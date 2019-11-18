#!/usr/bin/env bash

TMPDIR=~/tmp

# URL='https://hosts-file.net/download/hosts.zip'
URL='https://hosts-file.net/download/hosts.txt'
URL='https://hosts-file.net/download/HOSTS-Optimized.txt'
HOSTFILEADS=hosts-ads.txt

if [ -r /etc/hosts.orignal ]
then
    ls -l /etc/hosts
    head -10 /etc/hosts
    if cd ~/tmp
    then
        if [ ! -r ${HOSTFILEADS} ]
        then
            wget -c $URL -O ${HOSTFILEADS}
        fi

        if [ -e $HOSTFILEADS ]
        then
            if ! dos2unix $HOSTFILEADS
            then
                rm -f $HOSTFILEADS
                echo Error >&2
                exit
            fi
        fi

        if [ -r  ${HOSTFILEADS} ]
        then
            cp /etc/hosts.orignal $TMPDIR/hosts
            echo -e "\n\n\n" >> $TMPDIR/hosts
            sed -n '/BAD HOSTS BEGIN HERE/,$p' ${HOSTFILEADS} >> $TMPDIR/hosts
            sudo cp $TMPDIR/hosts /etc/hosts
        fi
    fi
    head -20 /etc/hosts
    ls -l /etc/hosts
fi
