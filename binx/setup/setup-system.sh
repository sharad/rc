#!/bin/bash

SSH_KEY_DUMP=$1
SITEDIR=/usr/local/
TMPDIR=~/setuptmp

function main()
{
    mkdir -p $SITEDIR/.repos/git/system/
    setup_apt_packages

    setup_ssh_keys "$SSH_KEY_DUMP"

    setup_git_repos
    setup_packages
    setup_misc
}


function setup_apt_packages()
{
    sudo apt install git-all openssl stow sbcl cl-clx-sbcl cl-quicklisp
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

function setup_packages()
{
    setup_clisp_packages
    setup_quicklisp_package
    setup_stumwpm_packages
}

function setup_quicklisp_package()
{
    # curl -O https://beta.quicklisp.org/quicklisp.lisp
    # curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
    # gpg --verify quicklisp.lisp.asc quicklisp.lisp
    # sbcl --load quicklisp.lisp

    sudo apt install cl-quicklisp

    sudo mkdir -p  $SITEDIR/share/common-lisp/source/
    sudo chown $USER.$USER -R $SITEDIR/share/common-lisp/source/

    # sbcl --load /usr/share/cl-quicklisp/quicklisp.lisp \
    #      --eval '(quicklisp-quickstart:install :path "'$SITEDIR/share/common-lisp/source/quicklisp'")'       \
    #      --eval '(ql:add-to-init-file)'                \
    #      --eval '(quit)'

    sbcl --load /usr/share/cl-quicklisp/quicklisp.lisp \
         --eval '(quicklisp-quickstart:install :path "'$SITEDIR/share/common-lisp/source/quicklisp'")'       \
         --eval '(quit)'
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

    sudo chown $USER.$USER -R $SITEDIR/share/common-lisp/source/

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
    sudo mkdir -p $SITEDIR/build
    sudo chown ${USER}.${USER} -R $SITEDIR/build
    if [ ! -d $SITEDIR/build/stumpwm ]
    then
        git clone https://sharad@github.com/sharad/stumpwm.git $SITEDIR/build/stumpwm
        cd $SITEDIR/build/stumpwm
        git checkout pa-point-timeout
        cd -
    fi

    cd $SITEDIR/build/stumpwm
    ./configure --prefix=/usr/local/stow/stumpwm --with-lisp=sbcl
    make
    sudo make install
    cd -
    cd /usr/local/stow
    stow stumpwm
    cd -
}

function setup_git_repos()
{
    mkdir -p $SITEDIR/.repos/git/system/

    if [ ! -d $SITEDIR/.repos/git/system/system ]
    then
	      git clone git@github.com:sharad/system.git $SITEDIR/.repos/git/system/system
    fi

    if [ ! -L $SITEDIR/.system ]
    then
	      rm -rf $SITEDIR/.system
	      ln -sf .repos/git/user/system/system $SITEDIR/.system
    fi
}

function setup_misc()
{
    cp -ar $SITEDIR/.repos/git/system/ubuntu/$SITEDIR/* $SITEDIR/
}

main

exit 0
