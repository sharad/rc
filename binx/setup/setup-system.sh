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

    # running setup_git_repos

    running setup_mail_server_client

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


    running setup_apache_usermod
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

    if [ ! -L /usr/local/bin/stumpwm -o ! -x /usr/local/stow/stumpwm/bin/stumpwm ]
    then

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
    else
        echo stumpwm already installed. >&2
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
    if [ ! -L /usr/local/bin/conkeror -o ! -x /usr/local/stow/conkeror/bin/conkeror ]
    then

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
            sudo sed -i 's@exec firefox@exec /opt/firefox/firefox@' $(readlink -m /usr/local/bin/conkeror)
        fi
        cd -
    else
        echo conkeror already installed. >&2
    fi
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

function setup_dovecot()
{
    sudo apt -y install dovecot-core dovecot-imapd mail-stack-delivery
    if [ -d $SITEDIR/.repos/git/system/system/ubuntu/etc/dovecot ]
    then
        if [ -d /etc/dovecot-ORG ]
        then
            echo /etc/dovecot-ORG already present
        else
            sudo cp -ar /etc/docvot /etc/dovecot-ORG
        fi
        sudo systemctl stop dovecot.service
        sudo sh -c "cp -ar $SITEDIR/.repos/git/system/system/ubuntu/etc/dovecot/* /etc/dovecot/"
        running sleep 2s
        sudo systemctl start dovecot.service

    else
        echo $SITEDIR/.repos/git/system/system/ubuntu/etc/dovecot donot exists. >&2
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
        sudo systemctl stop postfix.service
        sudo sh -c "cp $SITEDIR/.repos/git/system/system/ubuntu/etc/postfix/* /etc/postfix/"
        running sleep 2s
        sudo systemctl start postfix.service
    else
        echo $SITEDIR/.repos/git/system/system/ubuntu/etc/postfix donot exists. >&2
    fi

    # M4 TODO?

    for var in \
        OFFICE1_DOMAIN \
            OFFICE1_SUPERDOMAIN \
            OFFICE1_USER \
            OFFICE1_PASSWORD \
            \
            OFFICE2_DOMAIN \
            OFFICE2_SUPERDOMAIN \
            OFFICE2_USER \
            OFFICE2_PASSWORD \
            \
            EXTERNAL1_DOMAIN \
            EXTERNAL1_SUPERDOMAIN \
            EXTERNAL1_USER \
            EXTERNAL1_PASSWORD \
            \
            EXTERNAL2_DOMAIN \
            EXTERNAL2_SUPERDOMAIN \
            EXTERNAL2_USER \
            EXTERNAL2_PASSWORD \
            \
            MYHOSTNAME \
            MYHOSTFDN \
            \
            MYUSERNAME
    do
        if eval [ "x" != "x\$$var" ] && eval echo sed -e 's/\\\$'$var/\$$var/g /etc/postfix/tls_per_site /etc/postfix/transport /etc/postfix/sender_dependent_relayhost /etc/postfix/sasl_passwd
        then
            eval sudo sed -i -e 's/\\\$'$var/\$$var/g /etc/postfix/main.cf /etc/postfix/generic /etc/postfix/tls_per_site /etc/postfix/transport /etc/postfix/sender_dependent_relayhost /etc/postfix/sasl_passwd
        else
            echo varialbe $var not set >&2
        fi
    done

    sudo cp $SITEDIR/.repos/git/system/system/ubuntu/etc/aliases /etc/aliases
    cd /etc/
    sudo postmap aliases
    cd -
}

function setup_gnomekeyring_imaps_smtps()
{
    echo TODO setup_gnomekeyring_imaps_smtps for imaps and smtps
}

function setup_offlineimap()
{
    sudo apt -y install offlineimap

    local class=maildata       # later change it to mail data
    local maildatadir=~/.fa/localdirs/org/deps.d/model.d/machine.d/default/volumes.d/view.d/$class
    local reqfstype=reiserfs

    if [ -d $maildatadir ]
    then
        fstype="$(df --output=fstype ${maildatadir}/ | sed -n 2p)"
        if [ $reqfstype = "$fstype" ]
        then
           if [ -d ~/.maildir -a ~/.offlineimap ]
           then
               # M4 TODO?

               echo  make sure to copy correct ~/.offlineimaprc

               echo and copy ~/.emacs.d/autoconfig/gnus/newsrc.eld from some other machine.

               running setup_gnomekeyring_imaps_smtps

           else
               echo ~/.maildir ~/.offlineimap are not created. >&2
               echo chek them by >&2
               echo ls -l ~/.maildir ~/.offlineimap
               echo readlink -m ~/.maildir ~/.offlineimap
               echo create them by >&2
               echo mkdir -p "$(readlink -m ~/.maildir)" >&2
               echo mkdir -p "$(readlink -m  ~/.offlineimap)" >&2
           fi
        else
            echo $maildatadir/ is not mounted on $reqfstype, correct it. >&2
            return 1
        fi
    else
        echo maildatadir $maildatadir is broken correct, setup link in ~/.fa/localdirs/deps.d/model.d/machine.d/default/volumes.d/view.d >&2
        return 1
    fi

}

function setup_mail_notification()
{
    echo todo implement setup_mail_notification >&2
}

function setup_ldapsearch()
{
    echo todo implement setup_ldapsearch >&2

    echo M4 TODO .ldapsearh
}

function setup-_password()
{
    echo ~/.ldappass /etc/postfix/sasl_passwd etc
}

function setup_notmuch()
{
    # notmuch setup
    echo M4 TODO ~/.setup/notmuch-config.m4
}

function setup_mail_server_client()
{
    local class=maildata       # later change it to mail data
    local maildatadir=~/.fa/localdirs/org/deps.d/model.d/machine.d/default/volumes.d/view.d/$class
    local reqfstype=reiserfs

    if [ -d $maildatadir ]
    then
        fstype="$(df --output=fstype ${maildatadir}/ | sed -n 2p)"
        if [ $reqfstype = "$fstype" ]
        then
            running setup_res_dir
            running setup_dovecot
            running setup_postfix
            running setup_offlineimap
            running setup_mail_notification
            running setup_ldapsearch
            running setup_password

            echo also see if $HOST required to be added to office-host-names variable in ~/.fa/osetup/info.d/common/elisp/common-info.el >&2

        else
            echo $maildatadir/ is not mounted on $reqfstype, correct it. >&2
            return 1
        fi
    else
        echo maildatadir $maildatadir is broken correct, setup link in ~/.fa/localdirs/org/deps.d/model.d/machine.d/default/volumes.d/view.d >&2
        return 1
    fi
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

    local MOUNTPOINT=/srv/volumes/local/res01/reiser01
    local VOL_RESIER=vgres01
    local LVOL_RESIER=lvres01


    local class=maildata       # later change it to mail data
    local maildatadir=~/.fa/localdirs/org/deps.d/model.d/machine.d/default/volumes.d/view.d/$class
    local reqfstype=reiserfs

    if [ -d $maildatadir ]
    then
        fstype="$(df --output=fstype ${maildatadir}/ | sed -n 2p)"
        if [ $reqfstype != "$fstype" ]
        then

            sudo mkdir -p /srv
            if sudo vgs --noheadings -o vg_name $VOL_RESIER
            then
                sudo apt -y install reiserfsprogs
                if ! sudo lvs --noheadings -o vg_name,lv_name $VOL_RESIER/$LVOL_RESIER
                then
                    echo going to create 1G lv $LVOL_RESIER in vg $VOL_RESIER
                    if confirm "Should I create /dev/mapper/$VOL_RESIER-$LVOL_RESIER ?: "
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
                        echo Stopped the creatin of /dev/mapper/$VOL_RESIER-$LVOL_RESIER >&2
                    fi
                else
                    return 1
                fi                       # if ! sudo lvs --noheadings -o vg_name,lv_name $VOL_RESIER/$LVOL_RESIER
            else
                echo Warning: $VOL_RESIER volume group do not exists. >&2
                return 1
            fi                          # if sudo vgs --noheadings -o vg_name $VOL_RESIER

        else
            echo volume /dev/mapper/$VOL_RESIER-$LVOL_RESIER already present. >&2
            return 0
        fi
    else
        echo maildatadir $maildatadir is broken correct, setup link in ~/.fa/localdirs/org/deps.d/model.d/machine.d/default/volumes.d/view.d >&2
        return 1
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

function confirm()
{
    # call with a prompt string or use a default
    read -r -p "${1:-Are you sure? [y/N]} " response
    case "$response" in
        [yY][eE][sS]|[yY])
            true
            ;;
        *)
            false
            ;;
    esac
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
