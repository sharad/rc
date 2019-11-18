#!/usr/bin/env bash

DEBUG=1

SSH_KEY_DUMP=$1
TMPDIR=~/setuptmp

: APT_REPO_COMMPUNICATION=""

: DEB_EXTRA_PKG_VIRTUAL=""


DEB_EXTRA_PKG_FONTS="ttf-bitstream-vera"
DEB_EXTRA_PKG_LISP="cl-swank"

function main()
{
    mkdir -p $TMPDIR
    set_keyboard
    cd ~/

    set_docker_machine

    setup_apt_packages

    # make home dir and paradise in root ownership.

    rm -rf $TMPDIR
}

function set_docker_machine()
{
    STOWDIR_PATH=/usr/local/stow/docker-machine
    STOWDIR="$(basename $STOWDIR_PATH)"
    STOWDIR_TMPDIR=$TMPDIR/$STOWDIR

    if false && sudo apt install stow
    then
        mkdir -p $STOWDIR_TMPDIR/bin $STOWDIR_TMPDIR/etc/bash_completion.d $STOWDIR_TMPDIR/etc/zsh/completion
        if curl -L "https://github.com/docker/machine/releases/download/v0.10.0/docker-machine-$(uname -s)-$(uname -m)" > $STOWDIR_TMPDIR/bin/docker-machine &&
           curl -L 'https://github.com/docker/machine/blob/master/contrib/completion/bash/docker-machine-prompt.bash'   > $STOWDIR_TMPDIR/etc/bash_completion.d/docker-machine-prompt.bash &&
           curl -L 'https://github.com/docker/machine/blob/master/contrib/completion/bash/docker-machine-wrapper.bash'  > $STOWDIR_TMPDIR/etc/bash_completion.d/docker-machine-wrapper.bash &&
           curl -L 'https://github.com/docker/machine/blob/master/contrib/completion/bash/docker-machine.bash'          > $STOWDIR_TMPDIR/etc/bash_completion.d/docker-machine.bash &&
           curl -L 'https://github.com/docker/machine/raw/master/contrib/completion/zsh/_docker-machine'                > $STOWDIR_TMPDIR/etc/zsh/completion/_docker-machine
        then
            chmod +x "$STOWDIR_TMPDIR/bin/docker-machine"
            sudo rm -rf "$STOWDIR_PATH"
            sudo cp -ra "$STOWDIR_TMPDIR" "$STOWDIR_PATH"
            cd /usr/local/stow
            sudo stow "$STOWDIR"
            cd -
        fi
    fi
}

function setup_apt_packages()
{
    sudo apt install docker docker.io
    sudo apt install vagrant
}

function set_keyboard()
{
    if [ ! -f $TMPDIR/keymap ]
    then
        mkdir -p $TMPDIR
        wget 'https://raw.githubusercontent.com/sharad/rc/master/keymaps/Xmodmaps/xmodmaprc-swap-alt-ctrl-caps=alt' -O $TMPDIR/keymap
    fi
    xmodmap $TMPDIR/keymap
}


main

exit
