#!/usr/bin/env bash

if [ x"$@" = x"quit" ]
then
    exit 0
fi


# Not present in 1.5.4



# Override the previously set prompt.
# echo -en "\x00delim\x1f\\x1\n"
# echo -en "\x00prompt\x1fChange prompt\x1"
# echo -en "\x00prompt\x1fChange prompt\n"

# "\\0NAME\\x1fVAL"

echo -en "\x00delim\x1f\\x1\n"
echo -en "\x00prompt\x1fChange prompt\x1"

# echo -en "\x00delim\x1f\\x2\n"
# echo -en "\x00prompt\x1fChange prompt\x2"

echo -en "\nAAAA\x1"

# for a in {1..10}
# do
#     echo -en "$a\x1"
# done

# echo -en "new\nlUine\ntest\x1\n"

# echo -en "test\nnewline\ntest\x1"

# echo -en "\x1test\nnewline\ntest\x1"

# echo -en "quit"
