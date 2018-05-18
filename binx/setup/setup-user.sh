#!/bin/bash

DEBUG=1

SSH_KEY_DUMP=$1
TMPDIR=~/setuptmp

GIT_OPTION="-c core.sshCommand='ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no'"
RESOURCEPATH=".repos/git/main/resource"
USERORGMAIN="userorg/main"

APT_REPO_COMMPUNICATION="ppa:nilarimogard/webupd8"
APT_REPO_UTILS="ppa:yartsa/lvmeject"

APT_REPO_KOI="ppa:team-xbmc/ppa"

DEB_PKG_FIRST_INTERCATIVE_QA="macchanger postfix cyrus-clients lyskom-server console-data"
DEB_PKG_FIRST_INSTALL="zsh"
DEB_PKG_EMACS="elpa-magit elpa-magit-popup elpa-with-editor emacs-goodies-el enscript flim lm-sensors "
DEB_PKG_NECESSARY_MORE1="xaos xnee xnee-doc xzgv yatex zsh zsh-doc zutils screen tmux tmuxp byobu ccze shutdown-at-night sitesummary xterm rxvt-unicode-256color"
# TODO BUG set zsh as login shell
DEB_PKG_NECESSARY_MORE2="gnu-smalltalk-doc gnu-fdisk gnu-standards gnuit gnulib gnupg2 gnuplot-doc gvpe gtypist hello ht id-utils indent integrit jed latex-mk ledger libaws-doc"
##  hello-traditional
DEB_PKG_NECESSARY_MORE3="libcommoncpp2-doc libconfig-dev libsocket++-dev licenseutils lookup-el lyskom-server macchanger mboxgrep mew-beta mit-scheme-doc mmm-mode ocaml-doc oneliner-el org-mode-doc parted-doc pcb-common"
DEB_PKG_NECESSARY_MORE4="pinfo psgml qingy r-doc-info r5rs-doc semi sepia sharutils slime source-highlight spell ssed stow rlwrap teseq time trueprint turnin-ng units vera wcalc gnome-calculator wdiff wizzytex wysihtml-el"
DEB_PKG_GAME="gnugo"
DEB_PKGS_BACKUP="bup git-annex tahoe-lafs unison unison-all inotify-tools"
DEB_PKG_NECESSARY="git git-review legit git-extras git-flow git-sh git-extras git-crypt ecryptfs-utils openssl stow sbcl cl-clx-sbcl at gksu openssh-server rcs apt-src apt-file jargon cutils complexity-doc dejagnu diffutils extract festival ffe gccintro gddrescue geda-doc genparse gpodder gnutls-bin"
DEB_PKG_WITH_ERROR="edb"
DEB_PKG_APPEARANCE="lxappearance gnome-tweak-tool gnome-themes-standard libgtk-3-dev console-data gnome-session gnome-settings-daemon gnome-panel policykit-1-gnome dex"
DEB_PKG_VIRTURALMACHINE="xrdp rdesktop vncviewer remmina remmina-plugin-rdp virtualbox-dkms virtualbox-guest-x11 vagrant"
DEB_PKGS1="vim emacs25-lucid emacs emacs-goodies-el org-mode develock-el dash-el s-el zile keychain undistract-me"

DEB_PKGS2="rxvt-unicode-256color elscreen planner-el p7zip-full pdftk golang gocode gparted"
DEB_EXTRA_PKG1=" libpam-tmpdir xdg-utils xdg-user-dirs menu-xdg extra-xdg-menus obsession keyringer menu tree wipe xclip python3-secretstorage"
DEB_EXTRA_PKG_COMMUNICATION="pidgin pidgin-skype pidgin-skypeweb empathy empathy-skype purple-skypeweb telegram-purple pidgin-plugin-pack bitlbee tor"
DEB_EXTRA_PKG_VIRTUAL=""
DEB_EXTRA_PKG_FONTS="ttf-bitstream-vera texlive-latex-extra texlive-fonts-recommended"
DEB_EXTRA_PKG_LISP="cl-swank slime"
DEB_EXTRA_PKG2="homesick yadm macchanger xautolock suckless-tools xtrlock xbacklight xautomation ffmpeg"
DEB_EXTRA_PKG3="makepasswd libstring-mkpasswd-perl inotify-tools conky-all macchanger lm-sensors tidy xmlstarlet network-manager-openvpn-gnome duc xmldiff"
DEB_EXTRA_SEC_PKG1="systemd-ui " # policykit-1 policykit-1-gnome
DEB_DEV_PKG1="python-pip silversearcher-ag silversearcher-ag-el s-el ack-grep doxygen doxymacs libjson-glib-dev cmake"
DEB_EXTRA_PKG3_UTILS="system-config-lvm lvmeject adcli partclone gpodder parallel"
DEB_PKG_DEV="valgrind"
DEB_PKG_SYSTEM="cpuid inxi arandr bluez bluez-tools pavucontrol redshift daemontools god circus software-properties-common at hibernate ps-watcher daemonfs daemonize daemon "
DEB_PKG_TOOL_TEST="cyrus-clients swaks im namazu2-index-tools prayer-accountd prayer"
DEB_SYS_PKG1="duc baobab agedu tpb daemontools sysstat isag dos2unix powermanagement-interface grub2-splashimages grub2-themes-ubuntu-mate offlineimap libsecret-tools"
DEB_SYS_MAIL="dovecot-core dovecot-imapd ntpdate postfix augeas-tools augeas-lenses notmuch afew ldap-utils bbdb3 elpa-lbdb lsdb mu-cite"
DEB_DEV_GTD="tomboy zim anki mnemosyne mnemosyne-blog sqlitebrowser"
DEB_PKG_LEARNING="gpodder"
DEB_PKG_TOOL_GUI="osdsh osd-cat xosd-bin notify-osd notify-osd-icons xosd-bin gpointing-device-settings touchfreeze bash-completion libinput-tools keynav" # xserver-xorg-input-synaptics
DEB_PKG_XWM="xcompmgr autocutsel sakura"
DEB_PKG_XML="libxml2-utils xsltproc"
DEB_PKG_UTILS="gcalcli"
DEB_PKG_MEDIA="kodi"


PY_PIP_PKG="termdown"

function main()
{

    process_arg $@


    mkdir -p $TMPDIR

    set_keyboard

    cd ~/

    running setup_apt_packages

    running setup_ecrypt_private


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

    running setup_git_repos

    running setup_user_config_setup

    running setup_ssh_keys "$SSH_KEY_DUMP"

    running setup_download_misc

    running setup_login_shell

    running setup_dirs

    running setup_sourcecode_pro_font

    running setup_apache_usermod

    running setup_mail

    running setup_crontab

    running setup_spacemacs

    running setup_clib_installer

    running setup_clib_pkgs

    running setup_bpkg_installler

    running setup_bpkg_pkgs

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

function setup_apt_repo()
{
    if [ -r /etc/os-release ]
    then
        . /etc/os-release
        if [ ubuntu = $ID ]
        then
            read _ UBUNTU_VERSION_NAME <<< "$VERSION"
            echo "Running Ubuntu $UBUNTU_VERSION_NAME"
        else
            echo "Not running an Ubuntu distribution. ID=$ID, VERSION=$VERSION" >&2
            exit -1
        fi
    else
        echo "Not running a distribution with /etc/os-release available" >&2
    fi

    for repo in "$APT_REPO_COMMPUNICATION" "$APT_REPO_UTILS"
    do
        # /etc/apt/sources.list.d/nilarimogard-ubuntu-webupd8-xenial.list
        # echo repo=$repo
        REPO_NAME1="$(echo $repo | cut -d: -f2 | cut -d/ -f1)"
        REPO_NAME2="$(echo $repo | cut -d: -f2 | cut -d/ -f2)"

        REPO_FILE_PATH=/etc/apt/sources.list.d/${REPO_NAME1}-${ID}-${REPO_NAME2}-${VERSION_CODENAME}.list

        if [ "$DEBUG" ]
        then
            echo REPO_FILE_PATH=$REPO_FILE_PATH
        fi

        if [ ! -f "$REPO_FILE_PATH" ]
        then
            sudo add-apt-repository "$repo"
        else
            echo "$REPO_FILE_PATH" already added.
        fi
    done

}

function setup_apt_packages()
{
    setup_apt_repo


sudo apt -y autoremove
sudo apt -y autoclean
sudo apt -y update
sudo apt-file update
sudo apt -y autoremove
sudo apt -y autoclean
sudo apt -y upgrade
sudo apt -y autoremove
sudo apt -y autoclean
sudo apt -y autoremove
sudo apt -y autoclean





    sudo apt update

    for pkg in \
        "DEB_PKG_FIRST_INTERCATIVE_QA" \
        "DEB_PKG_FIRST_INSTALL" \
        "DEB_PKG_NECESSARY" \
				"DEB_PKGS1" \
				"DEB_PKGS2" \
				"DEB_EXTRA_PKG1" \
				"DEB_EXTRA_PKG2" \
				"DEB_EXTRA_PKG3" \
				"DEB_EXTRA_SEC_PKG1" \
				"DEB_DEV_PKG1" \
				"DEB_EXTRA_PKG_FONTS" \
				"DEB_EXTRA_PKG_LISP" \
				"DEB_EXTRA_PKG_COMMUNICATION" \
				"DEB_EXTRA_PKG_VIRTUAL" \
				"DEB_EXTRA_PKG3_UTILS" \
				"DEB_PKG_APPEARANCE" \
				"DEB_PKG_VIRTURALMACHINE" \
				"DEB_PKG_SYSTEM" \
				"DEB_PKG_DEV" \
				"DEB_PKG_EMACS" \
				"DEB_PKG_TOOL_TEST" \
				"DEB_SYS_PKG1" \
				"DEB_PKGS_BACKUP" \
				"DEB_PKG_GAME" \
				"DEB_PKG_NECESSARY_MORE1" \
				"DEB_PKG_NECESSARY_MORE2" \
				"DEB_PKG_NECESSARY_MORE3" \
				"DEB_PKG_NECESSARY_MORE4" \
        "DEB_DEV_GTD" \
        "DEB_PKG_LEARNING" \
        "DEB_PKG_TOOL_GUI" \
        "DEB_PKG_XWM" \
        "DEB_PKG_XML" \
        "DEB_PKG_UTILS"
    do
        if ! eval sudo apt -y install \$$pkg
        then
            for p in $(eval echo \$$pkg)
            do
                echo sudo apt -y install ${p}
                sudo apt -y install ${p}
            done
        fi
    done

    for $pkg in PY_PIP_PKG
    do
        eval sudo pip install $PY_PIP_PKG
    done
}

function setup_ecrypt_private()
{
    if ! mount | grep $HOME/.Private
    then
        sudo apt -y install ecryptfs-utils

        if [ ! -f ~/.ecryptfs/wrapped-passphrase ]
        then
	          ecryptfs-setup-private
        fi

        # TODO BUG check for changes in homedir
        sed -i 's@/Private@/.Private@' ~/.ecryptfs/Private.mnt
        echo $HOME/.Private > ~/.ecryptfs/Private.mnt

        ecryptfs-mount-private
    fi
}

function setup_tmp_ssh_keys()
{
    sudo apt -y install openssl
    SSH_KEY_ENC_DUMP=$1
    SSH_DIR=$2
    if [ "x$SSH_KEY_ENC_DUMP" != "x" -a -f "$SSH_KEY_ENC_DUMP" ]
    then
        ## bring the ssh keys
        if [ ! -r $TMPDIR/ssh/nosecure.d/ssh/keys.d/github ]
        then
	          mkdir -p $SSH_DIR
            # TODO BUG not working
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
        ## bring the ssh keys
    if [ ! -r ~/.osetup/nosecure.d/ssh/keys.d/github ]
    then
        if [ "x$SSH_KEY_ENC_DUMP" != "x" -a -f "$SSH_KEY_ENC_DUMP" ]
        then
            sudo apt -y install openssl
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
        else
            echo setup_ssh_keys: key file not provided or not exists.
        fi
    fi

    if ! ssh-add -l
    then
	      ssh-add ~/.osetup/nosecure.d/ssh/keys.d/github
    fi
}

function setup_git_repos()
{
    # RESOURCEPATH=".repos/git/main/resource"
    # USERORGMAIN="userorg/main"

    mkdir -p ~/${RESOURCEPATH}/
    if [ ! -d ~/${RESOURCEPATH}/userorg ]
    then
        git $GIT_OPTION clone --recursive  git@github.com:sharad/userorg.git ~/${RESOURCEPATH}/userorg
    else
        git $GIT_OPTION -C ~/${RESOURCEPATH}/userorg pull origin master
        git $GIT_OPTION -C ~/${RESOURCEPATH}/userorg submodule foreach git $GIT_OPTION pull origin master
        # git $GIT_OPTION -C ~/.repos/git submodule update --remote
    fi

    if true
    then

        if [ ! -L ~/.localdirs -a -d ~/.localdirs ]
        then
    	      rm -rf ~/.localdirs
	          ln -sf ${RESOURCEPATH}/${USERORGMAIN}/readwrite/public/user/localdirs ~/.localdirs
        fi

        if [ ! -L ~/.setup ]
        then
	          rm -rf ~/.setup
	          ln -sf .localdirs/rc.d/setup ~/.setup
        fi

        if [ ! -L ~/.osetup ]
        then
            rm -f ~/.osetup
            ln -sf .localdirs/rc.d/osetup ~/.osetup
        fi

        if mount | grep $HOME/.Private
        then
            if [ ! -d ~/.Private/secure.d -a -d ~/${RESOURCEPATH}/${USERORGMAIN}/readwrite/private/user/secure.d ]
            then
	              rm -rf ~/.Private/secure.d
	              cp -ra ~/${RESOURCEPATH}/${USERORGMAIN}/readwrite/private/user/secure.d ~/.Private/secure.d
            fi
        fi

        if [ ! -d ~/.pi -a -d ~/.setup/pi ]
        then
	          ln -s .setup/pi ~/.pi
	          ln -s ../${RESOURCEPATH}/${USERORGMAIN}/readwrite/private/user/orgp ~/.pi/org
        fi

        if [ ! -d ~/.emacs.d/.git ]
        then
	          if [ -d ~/.emacs.d ]
            then
                mv ~/.emacs.d ~/.emacs.d-old
            fi
	          ln -s ${RESOURCEPATH}/${USERORGMAIN}/readonly/public/user/spacemacs ~/.emacs.d
        fi

    fi
}

function setup_user_config_setup()
{
    RCHOME="$HOME/${RESOURCEPATH}/${USERORGMAIN}/readwrite/public/user/rc/_home/"
    if [ -d "${RCHOME}" ]
    then
	      if mkdir -p ~/_old_dot_filedirs
        then
	          # mv ~/.setup/_home/.setup $TMPDIR/Xsetup
	          cd "${RCHOME}"
	          for c in .[a-zA-Z^.^..]* *
	          do
                echo considering $c
                clink=$(readlink $c)
	              if [ "$c" != ".repos" -a "$c" != ".setup" -a "$c" != "." -a "$c" != ".." -a $clink != ".." ]
	              then
		                if [ -e ~/$c ]
		                then
                        if [ ! -L ~/$c -o "$(readlink ~/$c)" != "$(readlink $c)" ]
                        then

                            if [ ! -L ~/$c ] # backup
                            then
		                            running mv ~/$c ~/_old_dot_filedirs
                            fi

                            if [ ! -e ~/$c ]
                            then
		                            running cp -af $c ~/$c
                                # exit -1
                            elif [ -L ~/$c ]
                            then
                                running rm -f ~/$c
                                running cp -af $c ~/$c
                                # exit -1
                                # continue
                            fi
                            echo done setting up $c
                        else
                            echo not doing anything $c ~/$c
                        fi
                    else
                        running cp -af $c ~/$c
                        echo done setting up $c
		                fi
                else
                    echo not setting up $c
	              fi
	          done
	          # mv $TMPDIR/Xsetup ~/.setup/_home/.setup
	          cd -
        fi
    else
        echo "${RCHOME}" not exists >&2
    fi # if [ -d "${RCHOME}" ]

    if [ -d ~/.setup/_home/acyclicsymlinkfix ]
    then
        for symlnk in      ~/.setup/_home/acyclicsymlinkfix/.Volumes \
                           ~/.setup/_home/acyclicsymlinkfix/Desktop \
                           ~/.setup/_home/acyclicsymlinkfix/Downloads \
                           ~/.setup/_home/acyclicsymlinkfix/Music \
                           ~/.setup/_home/acyclicsymlinkfix/Pictures
        do
            cp -a $symlnk ~/
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

function setup_Documentation()  # TODO
{
    if [  ! -L ~/Documents -a "$(readlink -m ~/Documents)" != "$HOME/.repos/git/user/doc" ]
    then
        # rm -f ~/.LocalDirs.d/home.d/Documents
        # ln -s ../../.repos/git/user/doc ~/.LocalDirs.d/home.d/Documents
        rm -f ~/Documents
        cp -a ~/.setup/_home/Documents ~/
    fi
}

function setup_public_html()    # TODO
{
    if [  ! -L ~/public_html -o "$(readlink -m ~/public_html)" != "$HOME/.repos/git/user/doc/Public/Published/html" ]
    then
        # rm -f ~/.LocalDirs.d/home.d/public_html
        # ln -s Documents/Public/Published/html ~/.LocalDirs.d/home.d/public_html
        rm -f ~/public_html
        cp -a ~/.setup/_home/public_html ~/
    fi
}

function setup_mail()
{
    sudo apt -y install dovecot-core dovecot-imapd ntpdate postfix
    if [ -d ~/.system/ubuntu/etc/postfix ]
    then
        if [ ! -d /etc/postfix-ORG ]
        then
            sudo cp -ar /etc/postfix /etc/postfix-ORG
            for f in ~/.system/ubuntu/etc/postfix/*
            do
                b=$(basename $f)
                cp $f /etc/postfix/
            done
        fi

        if [ ! -d /etc/dovecot-ORG ]
        then
            sudo cp -ar /etc/dovecot /etc/dovecot-ORG
            sudo cp ~/.system/ubuntu/etc/dovecot/conf.d/10-mail.conf /etc/dovecot/conf.d/10-mail.conf
        fi
    else
        echo ~/.system/ubuntu/etc/postfix not exists >&2
    fi
}

function setup_crontab()
{
    m4 ~/.setup/crontab.m4 2>/dev/null | crontab
}

function setup_login_shell()
{
    curshell="$(getent passwd $USER | cut -d: -f7)"
    if [ "$curshell" != "/bin/zsh" ]
    then
        sudo apt -y install zsh
        chsh -s /bin/zsh
    fi
}

function setup_paradise()
{
    curhomedir="$(getent passwd $USER | cut -d: -f6)"
    if [ "$(basename $curhomedir)" != hell ]
    then
        sudo rm -rf $curhomedir/hell # if exists
        newhomedir=$curhomedir/hell
        sudo mv $curhomedir ${curhomedir}_tmp
        sudo mkdir -p $curhomedir
        sudo mv ${curhomedir}_tmp "$newhomedir"
        # sudo mkdir -p "$newhomedir"
        sudo usermod -d "$newhomedir" $USER
        echo first change home dir to $newhomedir
        exit -1
        export HOME="$newhomedir"
    fi
}

function setup_dirs()
{
    # running setup_paradise

    for l in ~/.osetup/dirs.d/model.d/*/*
    do
        if [ -L "$l" ]
        then
            r="$(dirname $l)/$(readlink $l)"
            if [ ! -L "$r" -a ! -d "$r" ]
            then
                running rm -f "$r"
                running mkdir -p "$r"
            fi
        fi
    done

    # setup_Documentation
    # setup_public_html

    # sudo chown root.root -R ~/.LocalDirs.d/
}

function setup_spacemacs()
{
    login_env_dir=~/.rsetup/login/env.d
    mkdir -p $login_env_dir
    if [ -f $login_env_dir/$HOST ]
    then
        if ! grep spacemacs $login_env_dir/$HOST
        then
            cat <<'EOF' >> $login_env_dir/$HOST
# EMACS_DIST_DIR=.xemacs
EMACS_DIST_DIR=.emacs.d
export EMACS_DIST_DIR

# EMACS_SERVER_NAME=general
EMACS_SERVER_NAME=spacemacs
export EMACS_SERVER_NAME
EOF
        fi
    fi
}

function setup_sourcecode_pro_font()
{
    # http://programster.blogspot.in/2014/09/ubuntu-install-source-code-pro.html
    local FONT_NAME="SourceCodePro"
    local URL="https://github.com/adobe-fonts/source-code-pro/archive/1.017R.tar.gz"

    if [ ! -d /usr/share/fonts/truetype/$FONT_NAME/ ]
    then
        mkdir /tmp/$FONT_NAME
        cd /tmp/$FONT_NAME
        wget $URL -O "`echo $FONT_NAME`.tar.gz"
        tar --extract --gzip --file ${FONT_NAME}.tar.gz
        sudo mkdir /usr/share/fonts/truetype/$FONT_NAME
        sudo cp -rf /tmp/$FONT_NAME/. /usr/share/fonts/truetype/$FONT_NAME/.
        fc-cache -f -v
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

function setup_clib_installer()
{
    sudo apt-get -y install libcurl4-gnutls-dev -qq
    if [ ! -d /usr/local/stow/clib/ ]
    then
        if git $GIT_OPTION clone https://github.com/clibs/clib.git $TMPDIR/clib
        then
            cd $TMPDIR/clib
            make PREFIX=/usr/local/stow/clib/
            sudo make PREFIX=/usr/local/stow/clib/ install
            cd /usr/local/stow && sudo stow clib
            cd -
            rm -rf $TMPDIR/clib
        fi
    else
        echo clib is already present. >&2
    fi
}

function install_clib_pkg()
{
    local pkgfull="$1"
    local pkg="$(basename $pkgfull)"
    if [ ! -d /usr/local/stow/$pkg ]
    then
        sudo sh -c "PREFIX=/usr/local/stow/$pkg clib install $pkgfull -o /usr/local/stow/$pkg"
        cd /usr/local/stow && sudo stow $pkg
        cd -
    else
        echo $pkgfull is already present. >&2
    fi
}

function setup_clib_pkgs()
{
    :
}

function setup_bpkg_installler()
{
    install_clib_pkg bpkg/bpkg
}

function install_bpkg_pkg()
{
    local pkgfull="$1"
    local pkg="$(basename $pkgfull)"
    if [ ! -d /usr/local/stow/$pkg ]
    then
        sudo mkdir -p "/usr/local/stow/$pkg/bin"
        sudo sh -c "PREFIX=/usr/local/stow/$pkg bpkg install -g $pkgfull"
        cd /usr/local/stow/ && sudo stow $pkg
        cd -
    else
        echo $pkgfull is already present. >&2
    fi
}

function setup_bpkg_pkgs()
{
    install_bpkg_pkg nevik/gitwatch
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

exit
