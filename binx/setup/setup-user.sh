#!/bin/bash

DEBUG=1

SSH_KEY_DUMP=$1
TMPDIR=~/setuptmp

APT_REPO_COMMPUNICATION="ppa:nilarimogard/webupd8"
APT_REPO_UTILS="ppa:yartsa/lvmeject"


DEB_PKG_EMACS="elpa-magit elpa-magit-popup elpa-with-editor emacs-goodies-el enscript flim lm-sensors"
DEB_PKG_NECESSARY_MORE1="xaos xnee xnee-doc xzgv yatex zsh zsh-doc zutils byobu ccze shutdown-at-night sitesummary"
# TODO BUG set zsh as login shell
DEB_PKG_NECESSARY_MORE2="gnu-smalltalk-doc gnu-fdisk gnu-standards gnuit gnulib gnupg2 gnuplot-doc gvpe gtypist hello ht id-utils indent integrit jed latex-mk ledger libaws-doc"
##  hello-traditional
DEB_PKG_NECESSARY_MORE3="libcommoncpp2-doc libconfig-dev libsocket++-dev licenseutils lookup-el lyskom-server macchanger mboxgrep mew-beta mit-scheme-doc mmm-mode ocaml-doc oneliner-el org-mode-doc parted-doc pcb-common"
DEB_PKG_NECESSARY_MORE4="pinfo psgml qingy r-doc-info r5rs-doc semi sepia sharutils slime source-highlight spell ssed stow rlwrap teseq time trueprint turnin-ng units vera wcalc wdiff wizzytex wysihtml-el"
DEB_PKG_GAME="gnugo"
DEB_PKGS_BACKUP="bup git-annex tahoe-lafs unison unison-all"
DEB_PKG_NECESSARY="git legit git-flow git-sh git-extras git-crypt ecryptfs-utils openssl stow sbcl cl-clx-sbcl at gksu openssh-server rcs apt-src jargon cutils complexity-doc dejagnu diffutils edb extract festival ffe gccintro gddrescue geda-doc genparse gpodder"
DEB_PKG_APPEARANCE="lxappearance gnome-tweak-tool gnome-themes-standard libgtk-3-dev console-data gnome-session gnome-settings-daemon gnome-panel"
DEB_PKG_VIRTURALMACHINE="xrdp rdesktop vncviewer remmina remmina-plugin-rdp virtualbox-dkms virtualbox-guest-x11 vagrant"
DEB_PKGS1="vim emacs emacs-goodies-el org-mode develock-el dash-el s-el zile keychain undistract-me"
DEB_PKGS2="rxvt-unicode-256color elscreen planner-el p7zip-full pdftk golang gocode"
DEB_EXTRA_PKG1=" libpam-tmpdir xdg-utils xdg-user-dirs menu-xdg extra-xdg-menus obsession keyringer menu tree wipe xclip"
DEB_EXTRA_PKG_COMMUNICATION="pidgin pidgin-skypeweb purple-skypeweb telegram-purple tor"
DEB_EXTRA_PKG_VIRTUAL=""
DEB_EXTRA_PKG_FONTS="ttf-bitstream-vera texlive-latex-extra texlive-fonts-recommended"
DEB_EXTRA_PKG_LISP="cl-swank slime"
DEB_EXTRA_PKG2="homesick yadm macchanger xautolock suckless-tools xtrlock xbacklight xautomation ffmpeg"
DEB_EXTRA_PKG3="makepasswd libstring-mkpasswd-perl inotify-tools conky-all macchanger lm-sensors tidy xmlstarlet network-manager-openvpn-gnome duc"
DEB_EXTRA_SEC_PKG1="systemd-ui " # policykit-1 policykit-1-gnome
DEB_DEV_PKG1="python-pip silversearcher-ag silversearcher-ag-el s-el ack-grep doxygen doxymacs"
DEB_EXTRA_PKG3_UTILS="system-config-lvm lvmeject adcli partclone gpodder"
DEB_PKG_DEV="valgrind"
DEB_PKG_SYSTEM="cpuid inxi arandr bluez bluez-tools pavucontrol redshift"
DEB_PKG_TOOL_TEST="cyrus-clients swaks im namazu2-index-tools prayer"
DEB_SYS_PKG1="duc baobab"
DEB_SYS_MAIL="dovecot-core dovecot-imapd ntpdate postfix"
DEB_DEV_GTD="tomboy zim anki mnemosyne mnemosyne-blog"
DEB_PKG_LEARNING="gpodder"


function main()
{
    mkdir -p $TMPDIR

    set_keyboard

    cd ~/

    echo Running setup_apt_packages
    setup_apt_packages

    echo Running setup_ecrypt_private
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

    echo Running setup_git_repos
    setup_git_repos

    echo Running setup_user_config_setup
    setup_user_config_setup

    echo Running setup_ssh_keys
    setup_ssh_keys "$SSH_KEY_DUMP"

    echo Running setup_download_misc
    setup_download_misc

    echo Running setup_login_shell
    setup_login_shell

    echo Running setup_dirs
    setup_dirs

    echo Running setup_sourcecode_pro_font
    setup_sourcecode_pro_font

    echo Running setup_mail
    setup_mail

    echo Running setup_spacemacs
    setup_spacemacs

    echo Running setup_clib_installer
    setup_clib_installer

    echo Running setup_clib_pkgs
    setup_clib_pkgs

    echo Running setup_bpkg_installler
    setup_bpkg_installler

    echo Running setup_bpkg_pkgs
    setup_bpkg_pkgs

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

    sudo apt update

    for pkg in \
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
        "DEB_PKG_LEARNING"
    do
        eval sudo apt -y install \$$pkg
    done
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
    mkdir -p ~/.repos
    if [ ! -d ~/.repos/git ]
    then
        git clone --recursive  git@github.com:sharad/userorg.git ~/.repos/git
    else
        git -C ~/.repos/git submodule foreach git pull origin master
        # git -C ~/.repos/git submodule update --remote
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

        if [ ! -L ~/.stumpwm.d/modules -a -d ~/.repos/git/system/stumpwm-contrib ]
        then
            rm -rf ~/.stumpwm.d/modules
            ln -s ../.repos/git/system/stumpwm-contrib ~/.stumpwm.d/modules
        fi


        if mount | grep $HOME/.Private
        then
            if [ ! -d ~/.Private/secure.d -a -d ~/.repos/git/user/secure.d ]
            then
	              rm -rf ~/.Private/secure.d
	              cp -ra ~/.repos/git/user/secure.d ~/.Private/secure.d
            fi
        fi

        if [ ! -d ~/.pi -a -d ~/.setup/pi ]
        then
	          ln -s .setup/pi ~/.pi
	          ln -s ../.repos/git/user/orgp ~/.pi/org
        fi

        if [ ! -d ~/.emacs.d/.git ]
        then
	          if [ -d ~/.emacs.d ]
            then
                mv ~/.emacs.d ~/.emacs.d-old
            fi
	          ln -s .repos/git/user/spacemacs ~/.emacs.d
        fi

    fi

}

function setup_user_config_setup()
{
    if [ -d ~/.repos/git/user/rc/_home/ ]
    then
	      if mkdir -p ~/_old_dot_filedirs
        then
	          # mv ~/.setup/_home/.setup $TMPDIR/Xsetup
	          cd ~/.repos/git/user/rc/_home/
	          for c in .[a-zA-Z^.^..]* *
	          do
                echo considering $c
	              if [ "$c" != ".repo" -a "$c" != "." -a "$c" != ".." ]
	              then
		                if [ -e ~/$c ]
		                then
                        if [ ! -L ~/$c -o "$(readlink ~/$c)" != "$(readlink $c)" ]
                        then
                            if [ ! -L ~/$c ]
                            then
		                            echo mv ~/$c ~/_old_dot_filedirs
                            fi
                            if [ ! -e ~/$c ]
                            then
		                            echo cp -af ~/.repos/git/user/rc/_home/$c ~/$c
                                exit -1
                            elif [ -L ~/$c ]
                            then
                                echo rm -f ~/$c
                                echo cp -af ~/.repos/git/user/rc/_home/$c ~/$c
                                # exit -1
                                continue
                            fi
                            echo done setting up $c
                        else
                            echo not doing anything ~/.repos/git/user/rc/_home/$c ~/$c
                        fi
                    else
                        echo cp -af ~/.repos/git/user/rc/_home/$c ~/$c
                        echo done setting up $c
		                fi
                else
                    : echo not setting up $c
	              fi
	          done
	          # mv $TMPDIR/Xsetup ~/.setup/_home/.setup
	          cd -
        fi
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
    if [  -d ~/.osetup/dirs.d/local.d/dirs.d/home -a ! -d ~/.osetup/dirs.d/local.d/dirs.d/home/Documents ]
    then
        ln -s ../.repos/git/user/doc ~/.osetup/dirs.d/local.d/dirs.d/home/Documents
    fi
}

function setup_public_html()
{
    if [  -d ~/.osetup/dirs.d/local.d/dirs.d/home ]
    then
        mkdir -p ~/.osetup/dirs.d/local.d/dirs.d/home/public_html
        if [  -L ~/.osetup/dirs.d/local.d/dirs.d/home/public_html/content ]
        then
            ln -s Documents/CreatedContent/gen ~/.osetup/dirs.d/local.d/dirs.d/home/public_html/content
        fi
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
                cp $f /etc/postfix/%b
            done
        fi

        if [ ! -d /etc/dovecot-ORG ]
        then
            cp -ar /etc/dovecot /etc/dovecot-ORG
            cp ~/.system/ubuntu/etc/dovecot/conf.d/10-mail.conf /etc/dovecot/conf.d/10-mail.conf
        fi
    else
        echo ~/.system/ubuntu/etc/postfix not exists >&2
    fi
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

function setup_dirs()
{
    curhomedir="$(getent passwd $USER | cut -d: -f6)/hell"
    if [ "$(basename $curhomedir)" != hell ]
    then
        newhomedir=$curhomedir/hell
        usermod -d "$newhomedir" $USER
        mv $curhomedir ${curhomedir}_tmp
        mkdir -p $curhomedir
        mv ${curhomedir}_tmp "$newhomedir"
        sudo mkdir -p "$newhomedir"
        export $HOME="$newhomedir"
    fi

    sudo mkdir -p  ~/../paradise
    # make home dir and paradise in root ownership.
    sudo chown root.root ~/../paradise

    if [ ! -d ~/.osetup/dirs.d/local.d/dirs.d/home ]
    then
        mkdir ~/.LocalDir
        ln -s ../../../../../../../.Local ~/.osetup/dirs.d/local.d/dirs.d/home
        if [ -d "~/.osetup/dirs.d/home.d" ]
        then
            cd ~/.osetup/dirs.d/home.d
            for de in *
            do
                if [ -L $de ]
                then
                    if [ ! -d "$(readlink -m $de)" ]
                    then
                        echo mkdir -p "$(readlink -m $de)"
                    fi
                fi
            done
        fi
    fi

    setup_Documentation
    setup_public_html
}

function setup_spacemacs()
{
    if [ -f ~/.rsetup/login/$HOST ]
    then
        if ! grep spacemacs ~/.rsetup/login/$HOST
        then
            cat <<'EOF' >> ~/.rsetup/login/$HOST
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


function setup_clib_installer()
{
    sudo apt-get install libcurl4-gnutls-dev -qq
    if [ ! -d /usr/local/stow/clib/ ]
    then
        if git clone https://github.com/clibs/clib.git $TMPDIR/clib
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

main

exit
