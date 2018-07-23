#!/bin/bash

SSH_KEY_DUMP=$1
SITEDIR=/usr/local
TMPDIR=~/setuptmp

DEB_PKG_SYSTEM="git openssl stow sbcl cl-clx-sbcl cl-quicklisp  openssh-server cl-swank libfixposix-dev zsh"
DEB_PKG_SYSTEM1="gparted xterm rxvt-unicode-256color"


function main()
{
    process_arg $@

    running setup_zsh
    running setup_paradise


    sudo mkdir -p $SITEDIR/.repos/
    sudo chown ${USER}.${USER} -R $SITEDIR/.repos/
    running setup_apt_packages

    running setup_ssh_keys "$SSH_KEY_DUMP"

    running setup_git_repos

    running setup_packages
    running setup_misc
}


function setup_apt_packages()
{
    sudo apt -y install git
}

function setup_apt_packages()
{
    # setup_apt_repo

    sudo apt update

    for pkg in \
        "$DEB_PKG_SYSTEM" \
            "$DEB_PKG_SYSTEM1"
    do
        eval sudo apt -y install $pkg
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

    running setup_clisp_packages
    running setup_quicklisp_package
    running setup_clisp_ql_packages
    running setup_stumwpm_packages
    running setup_stumwpm_contrib_packages
    running setup_conkeror_package

    running setup_postfix
    running setup_offlineimap
    running setup_apache_usermod
    running setup_res_dir
}

function setup_quicklisp_package()
{
    # curl -O https://beta.quicklisp.org/quicklisp.lisp
    # curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
    # gpg --verify quicklisp.lisp.asc quicklisp.lisp
    # sbcl --load quicklisp.lisp

    sudo apt -y install cl-quicklisp
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

    sudo apt -y install stow
    sudo mkdir -p $SITEDIR/share/common-lisp/source
    sudo mkdir -p $SITEDIR/share/common-lisp/systems
    # sudo mkdir -p $SITEDIR/share/common-lisp/source/sharad

    sudo chown ${USER}.${USER} -R $SITEDIR/share/common-lisp/

    if [ ! -d $SITEDIR/share/common-lisp/source/sharad ]
    then
        # git clone https://sharad@github.com/sharad/stumpwm.git $SITEDIR/share/common-lisp/source/sharad/stumpwm
        # cd $SITEDIR/share/common-lisp/source/sharad/stumpwm
        # git checkout pa-point-timeout
        # cd -
        ln -s $SITEDIR/.repos/git/packages/common-lisp/source/sharad $SITEDIR/share/common-lisp/source/sharad
        if [ -d $SITEDIR/share/common-lisp/source/sharad/stumpwm ]
        then
            cd $SITEDIR/share/common-lisp/source/sharad/stumpwm
            git checkout pa-point-timeout
            cd -
        fi
    fi
}

function setup_clisp_ql_packages()
{
    # https://www.quicklisp.org/beta/faq.html#local-project
    sudo mkdir -p $SITEDIR/share/common-lisp/source/quicklisp/local-projects/
    sudo chown ${USER}.${USER} -R $SITEDIR/share/common-lisp/source/quicklisp/local-projects

    if [ ! -d $SITEDIR/share/common-lisp/source/quicklisp/local-projects/sharad ]
    then
        ln -s $SITEDIR/.repos/git/packages/common-lisp/source/sharad $SITEDIR/share/common-lisp/source/quicklisp/local-projects/sharad
        # git clone https://sharad@github.com/sharad/stumpwm.git $SITEDIR/share/common-lisp/source/quicklisp/local-projects/sharad/stumpwm
        if [ -d $SITEDIR/share/common-lisp/source/quicklisp/local-projects/sharad/stumpwm ]
        then
            cd $SITEDIR/share/common-lisp/source/quicklisp/local-projects/sharad/stumpwm
            git checkout pa-point-timeout
            cd -
        fi
    fi
}

function setup_stumwpm_packages()
{
    sudo apt -y install autoconf stow texinfo cl-swank

    if [ ! -d $SITEDIR/build/stumpwm ]
    then
        # git clone https://sharad@github.com/sharad/stumpwm.git $SITEDIR/build/stumpwm
        ln -s $SITEDIR/.repos/git/packages/common-lisp/source/sharad/stumpwm $SITEDIR/build/stumpwm
        if [ -d $SITEDIR/build/stumpwm ]
        then
            cd $SITEDIR/build/stumpwm
            git checkout pa-point-timeout
            cd -
        fi
    fi

    if [ -d $SITEDIR/build/stumpwm ]
    then
        cd $SITEDIR/build/stumpwm
        git checkout pa-point-timeout
        mkdir -p $TMPDIR
        ./autogen.sh
        ./configure --prefix=/usr/local/stow/stumpwm --with-lisp=sbcl

        sbcl --eval '(ql:quickload "quickproject")' \
             --eval '(quit)'

        make
        sudo make install
        cd -
        cd /usr/local/stow
        sudo stow stumpwm
        cd -
    fi

}

function setup_stumwpm_contrib_packages()
{
    sudo apt -y install libfixposix-dev
    # https://www.quicklisp.org/beta/faq.html#local-project
    if [ ! -d $SITEDIR/share/common-lisp/source/quicklisp/local-projects ]
    then
        mkdir -p $SITEDIR/share/common-lisp/source/quicklisp/local-projects
    fi

    if [ ! -d $SITEDIR/share/common-lisp/source/quicklisp/local-projects/stumpwm-contrib ]
    then
        ln -s $SITEDIR/.repos/git/packages/common-lisp/source/stumpwm-contrib $SITEDIR/share/common-lisp/source/quicklisp/local-projects/stumpwm-contrib
        # git clone https://github.com/sharad/stumpwm-contrib.git $SITEDIR/share/common-lisp/source/quicklisp/local-projects/stumpwm-contrib
    fi
}

function setup_git_repos()
{
    # TODO [ISSUE] add code to handle upstream remote branch changes and merging to origin branch

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

    if [ ! -d /opt/firefox ]
    then
        FOXVER=56.0
        FOXURL="https://ftp.mozilla.org/pub/firefox/releases/${FOXVER}/linux-x86_64/en-US/firefox-${FOXVER}.tar.bz2"
        if [ -e /opt/firefox ]
        then
            sudo rm -rf /opt/firefox
        fi
        wget -c $FOXURL -O - | sudo tar  xjf - -C /opt
    fi

    # /opt, and change /usr/local/bin/conkeror file also.
    sudo apt -y install stow

    if [ ! -d $SITEDIR/build/conkeror ]
    then
	      mkdir -p $SITEDIR/build
	      git clone git://repo.or.cz/conkeror.git $SITEDIR/build/conkeror
    else
        git -C $SITEDIR/build/conkeror pull --rebase
    fi

    make PREFIX=/usr/local/stow/conkeror -C $SITEDIR/build/conkeror
    sudo make PREFIX=/usr/local/stow/conkeror -C $SITEDIR/build/conkeror install
    cd /usr/local/stow
    sudo stow conkeror
    if [ -r /usr/local/bin/conkeror ]
    then
        sed -i 's@exec firefox@exec /opt/firefox/firefox@' $(readlink -m /usr/local/bin/conkeror)
    fi
    cd -
}

function setup_paradise()
{
    if [ "$(getent passwd $USER | cut -d: -f6)" == "/home/$USER" ]
    then
        sudo mv /home/$USER "/home/_$USER"
        sudo mkdir -p /home/$USER/paradise
        sudo mv "/home/_$USER" /home/$USER/hell
        sudo chown $USER.$USER -R /home/$USER/hell
        if sudo usermod -d /home/$USER/hell $USER
        then
            kill -INT -1
            sleep 2
            kill -KILL -1
        fi
    fi
}

function setup_zsh()
{
    sudo apt -y install zsh
    if [ "$(getent passwd $USER | cut -d: -f7)" != "/bin/zsh" ]
    then
        chsh -s /bin/zsh
    fi
}

function setup_postfix()
{
    sudo apt -y install postfix
    if [ -d $SITEDIR/.repos/git/system/system/ubuntu/etc/postfix ]
    then
        if [ -d /etc/postfix-ORG ]
        then
            echo /etc/postfix-ORG already present
        else
            sudo cp -ar /etc/postfix /etc/postfix-ORG
        fi
        sudo sh -c "cp $SITEDIR/.repos/git/system/system/ubuntu/etc/postfix/* /etc/postfix/"
    else
        echo $SITEDIR/.repos/git/system/system/ubuntu/etc/postfix donot exists >&2
    fi
}

function setup_offlineimap()
{
    sudo apt -y install offlineimap
}


function setup_apache_usermod()
{
    if [ -r /etc/apache2/apache2.conf ]
    then
        if [ ! -d /usr/local/etc/apache ]
        then
            mkdir -p /usr/local/etc/
            cp -r ~/.system/ubuntu/usr/local/etc/apache /usr/local/etc/apache
        fi

        if ! grep /usr/local/etc/apache /etc/apache2/apache2.conf
        then
            cp /etc/apache2/apache2.conf $TMP/apache2.conf
            cat <<EOF >> $TMP/apache2.conf

# Include the virtual host configurations:
Include /usr/local/etc/apache/sites-enabled/*.conf

# Include generic snippets of statements
Include /usr/local/etc/apache/conf-enabled/*.conf

EOF
            sudo cp $TMP/apache2.conf /etc/apache2/apache2.conf
        fi
    fi
}

function setup_res_dir()
{

    local MOUNTPOINT=/srv/volumes/res01/reiser01
    local VOL_RESIER=vgres01
    local LVOL_RESIER=lvreiser01

    sudo mkdir -p /srv
    if sudo vgs --noheadings $VOL_RESIER
    then
       sudo apt -y install reiserfsprogs
       if ! sudo lvs --noheadings $VOL_RESIER/$LVOL_RESIER
       then
           if sudo lvcreate $VOL_RESIER -n $LVOL_RESIER -L 1G
           then
               if sudo mkreiserfs /dev/mapper/$VOL_RESIER-$LVOL_RESIER
               then
                   if sudo mkdir -p $MOUNTPOINT
                   then
                       if ! grep /dev/mapper/$VOL_RESIER-$LVOL_RESIER /etc/fstab
                       then
                           sudo cp /etc/fstab /etc/fstab-ORG
                           echo copied /etc/fstab into /etc/fstab-ORG
                           sudo bash -c 'echo  >> /etc/fstab'
                           sudo bash -c 'echo /dev/mapper/$VOL_RESIER-$LVOL_RESIER $MOUNTPOINT reiserfs defaults,auto 1 1 >> /etc/fstab'
                           sudo bash -c 'echo  >> /etc/fstab'
                       fi
                   fi
               fi
           fi
       fi
    else
        echo Warning: $VOL_RESIER volume group do not exists. >&2
    fi
}

function process_arg() {
    warn=1
    error=1

    if ! set -- $(getopt -n $pgm -o rnsehvw -- $@)
    then
        verbose Wrong command line.
    fi

    while [ $# -gt 0 ]
    do
        case $1 in
            (-r) recursive=1;;
            (-s) stash=1;;
            (-n) noaction="";;
            (-v) verbose=1;;
            (-w) warn="";;
            (-e) error="";;
            (-h) help;
                 exit;;
            (--) shift; break;;
            (-*) echo "$0: error - unrecognized option $1" 1>&2; help; exit 1;;
            (*)  break;;
        esac
        shift
    done
}

function running()
{
    echo running $@
    if [ ! $noaction ]
    then
        $@
    fi
}


main

exit 0
