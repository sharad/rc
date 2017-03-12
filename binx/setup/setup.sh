#!/bin/bash

SSH_KEY_DUMP=$1

function main()
{
    cd ~/
    setup_apt_packages
    setup_ecrypt_private
    setup_ssh_keys
    setup_git_repos

    setup_user_config_setup
}

function set_keyboard()
{
    wget 'https://raw.githubusercontent.com/sharad/rc/master/keymaps/Xmodmaps/xmodmaprc-swap-alt-ctrl-caps=alt' -O /tmp/keymap
    xmodmap /tmp/keymap
}

function setup_apt_packages()
{
    : sudo apt install git-all ecryptfs-utils openssl stow sbcl cl-clx-sbcl
}

function setup_ecrypt_private()
{
    if [ ! -f ~/.ecryptfs/wrapped-passphrase ]
    then
	ecryptfs-setup-private
    fi

    sed -i 's@/Private@/.Private@' ~/.ecryptfs/Private.mnt

    ecryptfs-mount-private
}

function setup_ssh_keys()
{
    ## bring the ssh keys
    if [ ! -r ~/tmp/ssh/nosecure.d/ssh/keys.d/github ]
    then
	mkdir -p ~/tmp/ssh
	openssl enc -in "$SSH_KEY_DUMP" -aes-256-cbc -d | tar -zxvf - -C ~/tmp/ssh
    fi

    if ! ssh-add -l
    then
	ssh-add ~/tmp/ssh/nosecure.d/ssh/keys.d/github
    fi
}

# exit
function setup_git_repos()
{
    mkdir -p ~/.repos/git/user ~/.repos/git/system

    if [ ! -d ~/.repos/git/user/rc ]
    then
	git clone git@github.com:sharad/rc.git ~/.repos/git/user/rc
    fi
    if [ ! -L ~/.repos/git/user/setup-trunk ]
    then
    	rm -rf ~/.repos/git/user/setup-trunk
	ln -sf rc ~/.repos/git/user/setup-trunk
    fi
    if [ ! -L ~/.setup-trunk ]
    then
	rm -rf ~/.setup-trunk
	ln -sf .repos/git/user/setup-trunk ~/.setup-trunk
    fi
    if [ ! -L ~/.setup ]
    then
	rm -rf ~/.setup
	ln -sf .setup-trunk ~/.setup
    fi

    if [ ! -d ~/.repos/git/system/system ]
    then
	git clone git@github.com:sharad/system.git ~/.repos/git/system/system
    fi

    if [ ! -L ~/.system ]
    then
	rm -rf ~/.system
	ln -sf .repos/git/user/system/system ~/.system
    fi

    if [ ! -d ~/.repos/git/system/stumpwm ]
    then
	git clone git@bitbucket.org:sh4r4d/stumpwm.git ~/.repos/git/system/stumpwm
    fi

    if [ ! -d ~/.repos/git/user/osetup ]
    then
	git clone git@bitbucket.org:sh4r4d/osetup.git ~/.repos/git/user/osetup
    fi

    if [ ! -L ~/.osetup ]
    then
	rm -rf ~/.osetup
	ln -sf .repos/git/user/osetup ~/.osetup
    fi

    if [ ! -d ~/.repos/git/system/sysinfo ]
    then
	git clone git@bitbucket.org:sh4r4d/sysinfo.git ~/.repos/git/system/sysinfo
    fi

    if [ ! -L ~/.sysinfo ]
    then
	rm -rf ~/.sysinfo
	ln -sf .repos/git/system/sysinfo ~/.sysinfo
    fi

    if [ ! -d ~/.repos/git/user/secure.d ]
    then
	git clone git@bitbucket.org:sh4r4d/secure.d.git ~/.repos/git/user/secure.d
    fi

    if [ ! -d ~/.Private/secure.d ]
    then
	rm -rf ~/.Private/secure.d
	cp -ra ~/.repos/git/user/secure.d ~/.Private/secure.d
    fi

    if [ ! -d ~/.repos/git/user/orgp ]
    then
	git clone git@bitbucket.org:sh4r4d/orgp.git ~/.repos/git/user/orgp
    fi

    if [ ! -d ~/.pi ]
    then
	if [ -d ~/.setup/pi ]
	then
	    ln -s .setup/pi ~/.pi
	    ln -s ../.repos/git/user/orgp ~/.pi/org
	fi
    fi
}

function setup_user_config_setup()
{
    if [ -d ~/.setup ]
    then
	mkdir -p ~/.old_dot_filedirs
	mv ~/.[a-zA-Z]* ~/.old_dot_filedirs/
	for d in .repos .osetup .setup .setup-trunk .sysinfo .system .pi
	do
	    echo mv ~/.old_dot_filedirs/$d ~/
	    mv ~/.old_dot_filedirs/$d ~/
	done
	mv ~/.setup/_home/.setup ~/.setup/_home/.setup_x
	cp -af ~/.setup/_home/.[a-zA-Z]* ~/
	mv ~/.setup/_home/.setup_x ~/.setup/_home/.setup 
    fi
}

main

exit
