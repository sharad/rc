#!/bin/bash

SSH_KEY_DUMP=$1
TMPDIR=~/setuptmp

DEB_PKGS1="vim emacs emacs-goodies-el org-mode"
DEB_PKGS2="rxvt-unicode-256color elscreen planner-el"

DEB_EXTRA_PKG1=" xdg-utils xdg-user-dirs menu-xdg extra-xdg-menus obsession keyringer menu tree wipe xclip"

DEB_EXTRA_PKG_FONTS="ttf-bitstream-vera"
DEB_EXTRA_PKG_LISP="cl-swank"

function main()
{
    mkdir -p $TMPDIR
    set_keyboard
    cd ~/
    setup_apt_packages
    setup_ecrypt_private
    if ! ssh-add -l
    then
	if [ "x$SSH_KEY_DUMP" = "x" ]
	then
	    echo ssh key encrypted dump no provided >&2
	    exit -1
        else
	    setup_tmp_ssh_keys "$SSH_KEY_DUMP" "$TMPDIR/ssh"
	fi
    fi

    if ! ssh-add -l
    then
	echo ssh key no available >&2
	exit -1
    fi

    setup_git_repos

    setup_user_config_setup

    # if [ "x$SSH_KEY_DUMP" = "x" ]
    # then
    #     echo ssh key encrypted dump no provided >&2
    #     exit -1
    # else
    setup_ssh_keys "$SSH_KEY_DUMP"
    # fi

    setup_download_misc

    # make home dir and paradise in root ownership.

    rm -rf $TMPDIR
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

function setup_apt_packages()
{
    sudo apt install git ecryptfs-utils openssl stow sbcl cl-clx-sbcl at gksu openssh-server
    sudo apt install $DEB_PKGS1
    sudo apt install $DEB_PKGS2
    sudo apt install $DEB_EXTRA_PKG1
    sudo apt install $DEB_EXTRA_PKG_FONTS
    sudo apt install $DEB_EXTRA_PKG_LISP
}

function setup_ecrypt_private()
{
    if ! mount | grep $HOME/.Private
    then
        sudo apt install ecryptfs-utils

        if [ ! -f ~/.ecryptfs/wrapped-passphrase ]
        then
	    ecryptfs-setup-private
        fi

        sed -i 's@/Private@/.Private@' ~/.ecryptfs/Private.mnt

        ecryptfs-mount-private
    fi
}

function setup_tmp_ssh_keys()
{
    sudo apt install openssl
    SSH_KEY_ENC_DUMP=$1
    SSH_DIR=$2
    if [ "x$SSH_KEY_ENC_DUMP" != "x" -a -f "$SSH_KEY_ENC_DUMP" ]
    then
        ## bring the ssh keys
        if [ ! -r $TMPDIR/ssh/nosecure.d/ssh/keys.d/github ]
        then
	    mkdir -p $SSH_DIR
	    openssl enc -in "$SSH_KEY_ENC_DUMP" -aes-256-cbc -d | tar -zxvf - -C $SSH_DIR
        fi

        if ! ssh-add -l
        then
	    ssh-add $SSH_DIR/nosecure.d/ssh/keys.d/github
        fi
    else
        echo setup_tmp_ssh_keys: key file not provided or not exists.
    fi
}

function setup_ssh_keys()
{
    SSH_KEY_ENC_DUMP=$1
    if [ "x$SSH_KEY_ENC_DUMP" != "x" -a -f "$SSH_KEY_ENC_DUMP" ]
    then
        ## bring the ssh keys
        if [ ! -r ~/.osetup/ssh/nosecure.d/ssh/keys.d/github ]
        then
            sudo apt install openssl
            SSH_KEY_ENC_DUMP=$1
            SSH_DIR=$2
            if ! mount | grep "$USER/.Private"
            then
                ecryptfs-mount-private
            fi

            if ! mount | grep "$USER/.Private"
            then
                if [ -d ~/.osetup ]
                then
                    if [ -d ~/.osetup/nosecure.d -a -L ~/.osetup/secure -a -d ~/.osetup/secure ]
                    then
                        if [ ! -e ~/.osetup/nosecure.d/ssh/authorized_keys ]
                        then
                            touch ~/.osetup/nosecure.d/ssh/authorized_keys
                        fi

                        if [ ! -e ~/.osetup/secure/ssh/known_hosts ]
                        then
                            touch ~/.osetup/secure/ssh/known_hosts
                        fi

                        openssl enc -in "$SSH_KEY_ENC_DUMP" -aes-256-cbc -d | tar -zxvf - -C ~/.osetup/
                    else
                        echo setup_ssh_keys: directories ~/.osetup~/.osetup/nosecure.d or ~/.osetup/secure not exists.
                    fi
                else
                    echo setup_ssh_keys: directory ~/.osetup not exists.
                fi
            else
                echo setup_ssh_keys: "$USER/.Private" not mounted. >&2
            fi
        fi

        if ! ssh-add -l
        then
	    ssh-add ~/.osetup/nosecure.d/ssh/keys.d/github
        fi
    else
        echo setup_ssh_keys: key file not provided or not exists.
    fi
}

function setup_git_repos()
{
    mkdir -p ~/.repos
    if [ ! -d ~/.repos/git ]
    then
        git clone --recursive  git@github.com:sharad/userorg.git ~/.repos/git
    else
        git -C ~/.repos/git submodule update --remote
    fi

    if true
    then
        if [ ! -L ~/.repos/git/user/setup-trunk -a -d ~/.repos/git/user/rc ]
        then
    	    rm -rf ~/.repos/git/user/setup-trunk
	    ln -sf rc ~/.repos/git/user/setup-trunk
        fi
        if [ ! -L ~/.setup-trunk -a -d ~/.repos/git/user/setup-trunk ]
        then
	    rm -rf ~/.setup-trunk
	    ln -sf .repos/git/user/setup-trunk ~/.setup-trunk
        fi
        if [ ! -L ~/.setup -a -d ~/.setup-trunk ]
        then
	    rm -rf ~/.setup
	    ln -sf .setup-trunk ~/.setup
        fi

        if [ ! -L ~/.system -a -d ~/.repos/git/user/system/system ]
        then
	    rm -rf ~/.system
	    ln -sf .repos/git/user/system/system ~/.system
        fi

        if [ ! -L ~/.stumpwm.d/modules -a -d ~/.repos/git/system/stumpwm-contrib ]
        then
            rm -rf ~/.stumpwm.d/modules
            ln -s ../.repos/git/system/stumpwm-contrib ~/.stumpwm.d/modules
        fi

        if [ ! -L ~/.osetup -a -d ~/.repos/git/user/osetup ]
        then
	    rm -rf ~/.osetup
	    ln -sf .repos/git/user/osetup ~/.osetup
        fi

        if [ ! -L ~/.sysinfo -a -d ~/.repos/git/system/sysinfo ]
        then
	    rm -rf ~/.sysinfo
	    ln -sf .repos/git/system/sysinfo ~/.sysinfo
        fi

        if mount | grep $HOME/.Private
        then
            if [ ! -d ~/.Private/secure.d -a -d ~/.repos/git/user/secure.d ]
            then
	        rm -rf ~/.Private/secure.d
	        cp -ra ~/.repos/git/user/secure.d ~/.Private/secure.d
            fi
        fi

        if [ ! -d ~/.pi -a -d -d ~/.setup/pi ]
        then
	    ln -s .setup/pi ~/.pi
	    ln -s ../.repos/git/user/orgp ~/.pi/org
        fi

        if [ ! -L ~/.opt -a -d ~/.repos/git/user/opt ]
        then
	    rm -rf ~/.opt
	    ln -sf .repos/git/user/opt ~/.opt
        fi

        # if [ ! -L ~/.opt -a -d ~/.repos/git/user/opt ]
        # then
	#     rm -rf ~/.opt
	#     ln -sf .repos/git/user/opt ~/.opt
        # fi
    fi

}

function setup_user_config_setup()
{
    if [ -d ~/.setup ]
    then
	mkdir -p ~/.old_dot_filedirs

	mv ~/.setup/_home/.setup $TMPDIR/Xsetup
	cd ~/.setup/_home/
	for c in .[a-zA-Z]* *
	do
            echo considering $c
	    if [ "$c" != ".setup" -a "$c" != "." -a "$c" != ".." ]
	    then
		if [ -e ~/$c ]
		then
                    if [ ! -L ~/$c ] || [ "$(readlink $c)" != "$(readlink $c)" ]
                    then
		        mv ~/$c ~/.old_dot_filedirs
		        cp -af $c ~/$c
                        echo done setting up $c
                    else
                        : echo not doing anything $c ~/$c
                    fi
                else
                    cp -af $c ~/$c
                    echo done setting up $c
		fi
            else
                : echo not setting up $c
	    fi
	done
	mv $TMPDIR/Xsetup ~/.setup/_home/.setup
	cd ~/

	for d in .repos .osetup .setup .setup-trunk .sysinfo .system .pi .config .gconf .ecryptfs
	do
	    if [ -e ~/.old_dot_filedirs/$d ]
	    then
		if [ ! -e ~/$d ]
		then
		    echo mv ~/.old_dot_filedirs/$d ~/
		    mv ~/.old_dot_filedirs/$d ~/
		fi
	    fi
	done
    fi
}

function setup_download_misc()
{
    if [ ! -f /usr/local/bin/p4 ]
    then
	wget 'https://www.perforce.com/downloads/free/p4' -O $TMPDIR/p4
	sudo cp $TMPDIR/p4 /usr/local/bin/p4
	sudo chmod +x /usr/local/bin/p4
    fi
}

function setup_sshkeys()
{
    :
}

function setup_Documentation()
{
    :
}

function setup_mail()
{
    :
}

main

exit
