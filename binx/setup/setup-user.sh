#!/bin/bash

SSH_KEY_DUMP=$1
TMPDIR=~/setuptmp

DEB_PKGS1="vim emacs emacs-goodies-el org-mode"
DEB_PKGS2="rxvt-unicode-256color elscreen planner-el"

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
	fi
	setup_ssh_keys "$SSH_KEY_DUMP"
    fi

    if ! ssh-add -l
    then
	echo ssh key no available >&2
	exit -1
    fi
    setup_git_repos

    setup_user_config_setup

    setup_download_misc

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
    SSH_KEY_ENC_DUMP=$1
    ## bring the ssh keys
    if [ ! -r $TMPDIR/ssh/nosecure.d/ssh/keys.d/github ]
    then
	      mkdir -p $TMPDIR/ssh
	      openssl enc -in "$SSH_KEY_ENC_DUMP" -aes-256-cbc -d | tar -zxvf - -C $TMPDIR/ssh
    fi

    if ! ssh-add -l
    then
	      ssh-add $TMPDIR/ssh/nosecure.d/ssh/keys.d/github
    fi
}

function setup_git_repos()
{
    mkdir -p ~/.repos/git/user ~/.repos/git/system

    if [ ! -d ~/.repos/git/user/rc ]
    then
	      git clone git@github.com:sharad/rc.git ~/.repos/git/user/rc
    fi
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

    if [ ! -d ~/.repos/git/system/system ]
    then
	      git clone git@github.com:sharad/system.git ~/.repos/git/system/system
    fi

    if [ ! -L ~/.system -a -d ~/.repos/git/user/system/system ]
    then
	      rm -rf ~/.system
	      ln -sf .repos/git/user/system/system ~/.system
    fi

    if [ ! -d ~/.repos/git/system/stumpwm ]
    then
	      git clone git@bitbucket.org:sh4r4d/stumpwm.git ~/.repos/git/system/stumpwm
    fi

    if [ ! -d ~/.repos/git/system/stumpwm-contrib ]
    then
	git clone https://github.com/stumpwm/stumpwm-contrib.git ~/.repos/git/system/stumpwm-contrib
    fi
    if [ ! -L ~/.stumpwm.d/modules -a -d ~/.repos/git/system/stumpwm-contrib ]
    then
        rm -rf ~/.stumpwm.d/modules
        ln -s ../.repos/git/system/stumpwm-contrib ~/.stumpwm.d/modules
    fi

    if [ ! -d ~/.repos/git/user/osetup ]
    then
	      git clone git@bitbucket.org:sh4r4d/osetup.git ~/.repos/git/user/osetup
    fi

    if [ ! -L ~/.osetup -a -d ~/.repos/git/user/osetup ]
    then
	      rm -rf ~/.osetup
	      ln -sf .repos/git/user/osetup ~/.osetup
    fi

    if [ ! -d ~/.repos/git/system/sysinfo ]
    then
	      git clone git@bitbucket.org:sh4r4d/sysinfo.git ~/.repos/git/system/sysinfo
    fi

    if [ ! -L ~/.sysinfo -a -d ~/.repos/git/system/sysinfo ]
    then
	      rm -rf ~/.sysinfo
	      ln -sf .repos/git/system/sysinfo ~/.sysinfo
    fi

    if [ ! -d ~/.repos/git/user/secure.d ]
    then
	      git clone git@bitbucket.org:sh4r4d/secure.d.git ~/.repos/git/user/secure.d
    fi

    if [ ! -d ~/.Private/secure.d -a -d ~/.repos/git/user/secure.d ]
    then
	      rm -rf ~/.Private/secure.d
	      cp -ra ~/.repos/git/user/secure.d ~/.Private/secure.d
    fi

    if [ ! -d ~/.repos/git/user/orgp ]
    then
	      git clone git@bitbucket.org:sh4r4d/orgp.git ~/.repos/git/user/orgp
    fi

    if [ ! -d ~/.pi -a -d -d ~/.setup/pi ]
    then
	ln -s .setup/pi ~/.pi
	ln -s ../.repos/git/user/orgp ~/.pi/org
    fi

    if [ ! -d ~/.repos/git/user/opt ]
    then
	      git clone git@bitbucket.org:sh4r4d/opt.git ~/.repos/git/user/opt
    fi

    if [ ! -L ~/.opt -a -d ~/.repos/git/user/opt ]
    then
	      rm -rf ~/.opt
	      ln -sf .repos/git/user/opt ~/.opt
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
                        echo not doing anything $c ~/$c
                    fi
                else
                    cp -af $c ~/$c
                    echo done setting up $c
		fi
            else
                echo not setting up $c
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

main

exit
