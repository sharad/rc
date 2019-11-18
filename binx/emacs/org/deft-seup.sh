#!/usr/bin/env zsh

DEFT_DIR=$1

if [ "$DEFT_DIR" ]
then
    DEFT_DIR=$HOME/Documents/CreatedContent/contents/org
fi

DEFT_DIR=$(readlink -m $DEFT_DIR)

DEFT_DIR_LEN=$(expr $#DEFT_DIR + 2)

if [ -d $DEFT_DIR ]
then
    for f ( $(find ${DEFT_DIR} -name '*.org') ) {
        if (( ${+norun} ))
        then
            echo ln -s $f $(echo $f | cut -c${DEFT_DIR_LEN}- | tr / - )
        else
            ln -s $f $(echo $f | cut -c${DEFT_DIR_LEN}- | tr / - )
        fi
    }
else
    echo $DEFT_DIR do not exists. >&2
fi
