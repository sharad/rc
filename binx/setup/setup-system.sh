#!/bin/bash -e

SSH_KEY_DUMP=$1
SITEDIR=/usr/local
TMPDIR=~/setuptmp

DEB_PKG_SYSTEM="git openssl stow sbcl cl-clx-sbcl cl-quicklisp  openssh-server cl-swank"
DEB_PKG_SYSTEM1="gparted"


function main()
{
    sudo mkdir -p $SITEDIR/.repos/git/system/
    sudo chown ${USER}.${USER} -R $SITEDIR/.repos/git/system/
    setup_apt_packages

    setup_ssh_keys "$SSH_KEY_DUMP"

    setup_git_repos
    setup_packages
    setup_misc
}


function setup_apt_packages()
{
    sudo apt install git
}

function setup_apt_packages()
{
    # setup_apt_repo

    sudo apt update

    for pkg in \
        "$DEB_PKG_SYSTEM" \
            "$DEB_PKG_SYSTEM1"
    do
        eval sudo apt install $pkg
    done
}


function setup_ssh_keys()
{
    SSH_KEY_ENC_DUMP=$1
    if ! ssh-add -l
    then
        if [ "x" != "x$SSH_KEY_ENC_DUMP" ]
        then
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
        fi
    fi
}

function setup_packages()
{
    sudo mkdir -p $SITEDIR/build
    sudo chown ${USER}.${USER} -R $SITEDIR/build

    setup_clisp_packages
    setup_quicklisp_package
    setup_stumwpm_packages
    setup_conkeror_package

    setup_postfix
    setup_res_dir
}

function setup_quicklisp_package()
{
    # curl -O https://beta.quicklisp.org/quicklisp.lisp
    # curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
    # gpg --verify quicklisp.lisp.asc quicklisp.lisp
    # sbcl --load quicklisp.lisp

    sudo apt install cl-quicklisp
    sudo mkdir -p  $SITEDIR/share/common-lisp/source/
    sudo chown ${USER}.${USER} -R $SITEDIR/share/common-lisp/source/

    # sbcl --load /usr/share/cl-quicklisp/quicklisp.lisp \
    #      --eval '(quicklisp-quickstart:install :path "'$SITEDIR/share/common-lisp/source/quicklisp'")'       \
    #      --eval '(ql:add-to-init-file)'                \
    #      --eval '(quit)'
    if [ ! -d $SITEDIR/share/common-lisp/source/quicklisp ]
    then
        sbcl --load /usr/share/cl-quicklisp/quicklisp.lisp \
             --eval '(quicklisp-quickstart:install :path "'$SITEDIR/share/common-lisp/source/quicklisp/quicklisp'")' \
             --eval '(quit)'
    fi
    # (quicklisp-quickstart:install)
}

function setup_clisp_packages()
{
    mkdir -p ~/.config/common-lisp/source-registry.conf.d/
    echo '(:tree #p"'$SITEDIR/share/common-lisp/'")' > ~/.config/common-lisp/source-registry.conf.d/projects.conf

    sudo apt install stow
    sudo mkdir -p $SITEDIR/share/common-lisp/source
    sudo mkdir -p $SITEDIR/share/common-lisp/systems
    sudo mkdir -p $SITEDIR/share/common-lisp/source/sharad

    sudo chown ${USER}.${USER} -R $SITEDIR/share/common-lisp/

    if [ ! -d $SITEDIR/share/common-lisp/source/sharad/in.net.sharad.utils ]
    then
        git clone git@bitbucket.org:sh4r4d/in.net.sharad.utils $SITEDIR/share/common-lisp/source/sharad/in.net.sharad.utils
    fi

    if [ ! -d $SITEDIR/share/common-lisp/source/sharad/pa ]
    then
        git clone git@bitbucket.org:sh4r4d/pa $SITEDIR/share/common-lisp/source/sharad/pa
    fi

    if [ ! -d $SITEDIR/share/common-lisp/source/sharad/stumpwm ]
    then
        git clone https://sharad@github.com/sharad/stumpwm.git $SITEDIR/share/common-lisp/source/sharad/stumpwm
        cd $SITEDIR/share/common-lisp/source/sharad/stumpwm
        git checkout pa-point-timeout
        cd -
    fi
}

function setup_stumwpm_packages()
{
    sudo apt install autoconf texinfo cl-swank

    if [ ! -d $SITEDIR/build/stumpwm ]
    then
        git clone https://sharad@github.com/sharad/stumpwm.git $SITEDIR/build/stumpwm
        cd $SITEDIR/build/stumpwm
        git checkout pa-point-timeout
        cd -
    fi

    cd $SITEDIR/build/stumpwm
    ./autogen.sh
    ./configure --prefix=/usr/local/stow/stumpwm --with-lisp=sbcl
    make
    sudo make install
    cd -
    cd /usr/local/stow
    sudo stow stumpwm
    cd -
}

function setup_git_repos()
{
    sudo mkdir -p $SITEDIR/.repos
    sudo chown ${USER}.${USER} -R $SITEDIR/.repos

    if [ ! -d $SITEDIR/.repos/git ]
    then
        git clone --recursive git@github.com:sharad/usrlocalorg.git $SITEDIR/.repos/git
    else
        git -C $SITEDIR/.repos/git submodule update --remote
    fi
}

function setup_misc()
{
    sudo cp -ar $SITEDIR/.repos/git/system/system/ubuntu/$SITEDIR/* $SITEDIR/


    cd $SITEDIR/.repos/git/system/system/ubuntu/usr/share
    for f in applications/stumpwm.desktop \
             gnome-session/sessions/stumpwm.session \
             keymaps/i386/include/sharad.inc.gz \
             xsessions/stumpwm.desktop \
             xsessions/stumpwm-gnome.desktop
    do
	if [ ! -e /usr/share/$f ]
	then
            echo sudo mkdir -p /usr/share/$(dirname $f)
            echo sudo cp -i $SITEDIR/.repos/git/system/system/ubuntu/usr/share/$f /usr/share/$f
            sudo mkdir -p /usr/share/$(dirname $f)
            sudo cp -i $SITEDIR/.repos/git/system/system/ubuntu/usr/share/$f /usr/share/$f
	fi
    done
    cd -

    cd $SITEDIR/.repos/git/system/system/ubuntu/usr/local/bin
    for f in conkeror-old gnome-session-stumpwm  userifup  x-session-stumpwm
    do
	if [ ! -e /usr/local/bin/$f ]
	then
            sudo cp -i $SITEDIR/.repos/git/system/system/ubuntu/usr/local/bin/$f /usr/local/bin/$f
	fi
    done
    cd -
}

function setup_conkeror_package()
{

    if [ ! -d $SITEDIR/build/conkeror ]
    then
	mkdir -p $SITEDIR/build
	git clone git://repo.or.cz/conkeror.git $SITEDIR/build/conkeror
    fi

    make PREFIX=/usr/local/stow/conkeror -C $SITEDIR/build/conkeror
    sudo make PREFIX=/usr/local/stow/conkeror -C $SITEDIR/build/conkeror install
    cd /usr/local/stow
    sudo stow conkeror
    cd -
}

setup_postfix()
{
    :
}

function setup_res_dir()
{
    sudo mkdir -p /srv
    if sudo vgs --noheadings vgres01
    then
       sudo apt install reiserfsprogs
       if ! sudo lvs --noheadings vgres01/lvreiser01
       then
           if sudo lvcreate vgres01 -n lvreiser01 -L 1G
           then
               if sudo mkreiserfs /dev/mapper/vgres01-lvreiser01
               then
                   if sudo mkdir -p /srv/res//vgres01/lvreiser01
                   then
                       if ! grep /srv/res/vgres01/lvreiser01 /etc/fstab
                       then
                           sudo cp /etc/fstab /etc/fstab-ORG
                           sudo echo  >> /etc/fstab
                           sudo echo /dev/mapper/vgres01-lvreiser01 /srv/res/vgres01/lvreiser01 reiserfs defaults,auto 1 1 >> /etc/fstab
                           sudo echo  >> /etc/fstab
                       fi
                   fi
               fi
           fi
       fi
    else
        echo vgres01 volume group do not exists. >&2
    fi
}


main

exit 0
