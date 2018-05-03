#!/bin/bash

TMPDIR=~/tmp

# URL='https://hosts-file.net/download/hosts.zip'
URL='https://hosts-file.net/download/hosts.txt'
HOSTFILEADS=$(basename $URL)

if [ -r /etc/hosts.orignal ]
then
    ls -l /etc/hosts
    head -10 /etc/hosts
    if cd ~/tmp
    then
        if [ ! -r ${HOSTFILEADS} ]
        then
            if wget -c $URL
            then
                : dos2unix $HOSTFILEADS || rm -f $HOSTFILEADS
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
