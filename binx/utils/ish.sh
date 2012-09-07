#!/bin/zsh

RUNSHELL=bash

exec $RUNSHELL -i <<EOF
$@ < /dev/tty
exec <> /dev/tty
EOF

