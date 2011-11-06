#!/bin/zsh

bash -i <<EOF
exec < /dev/tty
$@
exec <> /dev/tty
EOF

