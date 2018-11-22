#!/bin/bash

DEBUG=1
export GIT_DISCOVERY_ACROSS_FILESYSTEM=1

SSH_KEY_DUMP=$1
TMPDIR=~/setuptmp

logicaldirs=(config deletable longterm preserved shortterm maildata)

dataclassname=data
homeclassname=home

if [ -r ~/.ssh/authorized_keys ]
then
    # GIT_SSH_OPTION="ssh -o UserKnownHostsFile=~/.ssh/authorized_keys -o StrictHostKeyChecking=yes"
    GIT_SSH_OPTION="ssh -o UserKnownHostsFile=~/.ssh/authorized_keys"
else
    GIT_SSH_OPTION="ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
fi

RESOURCEPATH=".repos/git/main/resource"
USERORGMAIN="userorg/main"

APT_REPO_COMMPUNICATION="ppa:nilarimogard/webupd8"
APT_REPO_UTILS="ppa:yartsa/lvmeject ppa:mikhailnov/pulseeffects"

APT_REPO_KOI="ppa:team-xbmc/ppa"

DEB_PKG_FIRST_INTERCATIVE_QA="macchanger postfix cyrus-clients lyskom-server console-data tlp"
DEB_PKG_FIRST_INSTALL="zsh"
DEB_PKG_EMACS="elpa-magit elpa-magit-popup elpa-magit-annex elpa-magithub magit elpa-with-editor emacs-goodies-el enscript flim lm-sensors preload pandoc automake g++ gcc libpng-dev libpoppler-dev libpoppler-glib-dev libpoppler-private-dev libz-dev make pkg-config elpa-projectile elpa-ghub elpa-ghub+ git-el" #
DEB_PKG_MESSAGING="namazu2 mhc x-face-el compface"
DEB_PKG_NECESSARY_MORE1="xaos xnee xnee-doc xzgv yatex zsh zsh-doc zutils screen tmux tmuxp byobu landscape-common update-motd ccze shutdown-at-night sitesummary xterm rxvt-unicode-256color cifs-utils"
# TODO BUG set zsh as login shell
DEB_PKG_NECESSARY_MORE2="gnu-smalltalk-doc gnu-fdisk gnu-standards gnuit gnulib gnupg2 gpa gnuplot-doc gvpe gtypist hello ht id-utils indent integrit jed latex-mk ledger libaws-doc rar"
##  hello-traditional
hDEB_PKG_NECESSARY_MORE3="libcommoncpp2-doc libconfig-dev libsocket++-dev licenseutils lookup-el lyskom-server macchanger mboxgrep mit-scheme-doc mmm-mode ocaml-doc oneliner-el org-mode-doc parted-doc pcb-common moreutils nmap zenmap"
DEB_PKG_NECESSARY_MORE4="pinfo psgml qingy r-doc-info r5rs-doc semi sepia sharutils slime source-highlight spell ssed stow rlwrap teseq time trueprint turnin-ng units vera wcalc gnome-calculator wdiff wizzytex wysihtml-el"
DEB_PKG_GAME="gnugo"
DEB_PKGS_BACKUP="bup git-annex tahoe-lafs unison unison-all inotify-tools"
DEB_PKG_NECESSARY="git git-review legit git-extras git-flow git-sh git-extras git-crypt ecryptfs-utils openssl stow sbcl cl-clx-sbcl at gksu openssh-server sshpass rcs apt-src flatpak apt-file jargon cutils complexity-doc dejagnu diffutils extract festival ffe gccintro gddrescue geda-doc genparse gpodder gnutls-bin pinentry-gnome3 pinentry-tty pinentry-curses mew-beta mew-beta-bin kwalletcli scdaemon kleopatra pinentry-x2go" # mew-bin
DEB_PKG_WITH_ERROR="edb"
DEB_PKG_APPEARANCE="lxappearance gnome-tweak-tool gnome-themes-standard libgtk-3-dev console-data gnome-session gnome-settings-daemon gnome-panel policykit-1-gnome dex"
DEB_PKG_VIRTURALMACHINE="xrdp rdesktop vncviewer remmina remmina-plugin-rdp virtualbox-dkms virtualbox-guest-x11 vagrant"
DEB_PKGS1="vim emacs25-lucid emacs emacs-goodies-el org-mode develock-el dash-el s-el zile keychain undistract-me rpm"

DEB_PKGS2="rxvt-unicode-256color elscreen planner-el p7zip-full pdftk golang gocode gparted"
DEB_EXTRA_PKG1=" libpam-tmpdir xdg-utils xdg-user-dirs menu-xdg extra-xdg-menus obsession keyringer menu tree wipe xclip python3-secretstorage copyq parcellite clipit diodon dunst zathura apvlv udiskie xsel xfce4-clipman rofi shellex"
DEB_EXTRA_PKG_COMMUNICATION="pidgin pidgin-skype pidgin-skypeweb empathy empathy-skype purple-skypeweb telegram-purple pidgin-plugin-pack bitlbee tor"
DEB_EXTRA_PKG_VIRTUAL=""
DEB_EXTRA_PKG_FONTS="ttf-bitstream-vera texlive-latex-extra texlive-fonts-recommended"
DEB_EXTRA_PKG_LISP="cl-swank slime"
DEB_EXTRA_PKG2="homesick yadm numlockx macchanger xautolock suckless-tools xtrlock xbacklight xautomation ffmpeg"
DEB_EXTRA_PKG3="makepasswd libstring-mkpasswd-perl inotify-tools conky-all macchanger lm-sensors tidy xmlstarlet network-manager-openvpn-gnome duc xmldiff"
DEB_EXTRA_SEC_PKG1="systemd-ui realmd sssd sssd-tools samba-common krb5-user packagekit samba-common-bin samba-libs adcli ntp winbind krb5-kdc krb5-config" # policykit-1 policykit-1-gnome , #  chrony (conflict with ntp)
DEB_DEV_PKG1="python-pip silversearcher-ag silversearcher-ag-el global cscope codequery seascope xcscope-el s-el ack-grep doxygen doxymacs libjson-glib-dev npm cmake uncrustify pasystray spacefm-gtk3 thunar thunar-volman pcmanfm xfce4-powermanager xfce4-notifyd ycmd fasd agda opam plsense"
DEB_EXTRA_PKG3_UTILS="system-config-lvm lvmeject adcli partclone gpodder parallel libpam-fprintd fprint-demo"
# https://www.cyberciti.biz/faq/removing-password-from-pdf-on-linux/
DEB_PKG_DEV="valgrind libxml2-dev gjs seed-webkit2 xpdf-utils ghostscript pdftk qpdf"
DEB_PKG_SYSTEM="cpuid inxi arandr bluez bluez-tools redshift daemontools god circus software-properties-common at hibernate ps-watcher daemonfs daemonize daemon slop scrot"
DEB_PKG_TOOL_TEST="cyrus-clients swaks im namazu2-index-tools prayer-accountd prayer"
DEB_SYS_PKG1="duc baobab agedu tpb daemontools sysstat isag dos2unix powermanagement-interface grub2-splashimages grub2-themes-ubuntu-mate offlineimap libsecret-tools"
# https://linuxconfig.org/fetch-stock-quotes-with-perl-finance-quote-module
DEB_SYS_MAIL="dovecot-core dovecot-imapd mail-stack-delivery ntpdate postfix augeas-tools augeas-lenses notmuch afew ldap-utils bbdb3 elpa-lbdb lsdb mu-cite libfinance-quote-perl mail-notification"
DEB_DEV_GTD="tomboy zim anki mnemosyne mnemosyne-blog sqlitebrowser"
DEB_PKG_LEARNING="gpodder"
DEB_PKG_TOOL_GUI="lightdm osdsh osd-cat xosd-bin notify-osd notify-osd-icons xosd-bin gpointing-device-settings touchfreeze bash-completion libinput-tools keynav feh geeqie" # xserver-xorg-input-synaptics
DEB_PKG_XWM="xcompmgr autocutsel sakura"
DEB_PKG_XML="libxml2-utils xsltproc docbook5-xml docbook-xsl-ns"
DEB_PKG_UTILS="gcalcli newsbeuter"
DEB_PKG_MEDIA="libavcodec-extra pulseeffects pavucontrol pulseaudio-module-gconf pulseaudio-equalizer vokoscreen pulseaudio-utils pulsemixer kodi sox mpg123 mpg321 vlc"
DEB_PKG_WINDOW="smbclient python3-smbc python-smbc"
DEB_PKG1_NET="network-manager-fortisslvpn openfortivpn python-pyftpdlib python3-pyftpdlib "
DEB_PKG_DOC="wv"
DEB_PKG_DOC_PUB="hugo jekyll"

PY_PIP_PKG="termdown "
NODE_PKG="tern "

function main()
{

    trap setup_finish EXIT SIGINT SIGTERM

    process_arg $@

    mkdir -p $TMPDIR

    set_keyboard

    cd ~/

    running setup_apt_packages

    running setup_ecrypt_private

    setup_tmp_ssh_keys "$TMPDIR/ssh" "$SSH_KEY_DUMP"

    if ! ssh-add -l
    then
	      echo ssh key no available >&2
	      exit -1
    fi

    # will set the ~/.setup also
    running setup_git_repos

    running setup_config_dirs

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

function setup_finish()
{
    rm -rf $TMPDIR
}

function setup_make_link()
{
    local target=$1
    local link=$2

    local rtarget="$target"
    if [ "$rtarget" != "${rtarget#/}" ]
    then
        verbose target $target is absolute path. >&2
    else
        verbose target $target is relative path. >&2
        rtarget="$(dirname $link)/$rtarget"
    fi

    if [ ! -L $link -o "$(readlink -m $link)" != "$(readlink -m $rtarget )" ]
    then
        if [ -e $link ]
        then
            if [ ! -L $link ]
            then
                warn link $link is not a link >&2
            else
                warn $link is pointing to  $(readlink $link) >&2
                warn while it should point to "$(readlink -m $rtarget )" >&2
            fi
            warn removing $link
            running mv $link ${link}-BACKUP
        else
            verbose $link do not exists >&1
        fi
        running rm -f  $link
        running ln -sf $target $link
    else
        verbose $link is correctly pointing to "$(readlink -m $rtarget )" is equal to $target
    fi
}

function setup_copy_link()
{
    local link=$1
    local target=$2

    if [ -d $target -a ! -L $target ]
    then
        echo target $traget can not be a directory >&2
        exit -1
    fi
    if [ -L "$link" ]
    then
        if [ ! -L $target -o "$(readlink -m $link)" != "$(readlink -m $target)" ]
        then
            if [ -e $target ]
            then
                if [ ! -L $target ]
                then
                    warn link $target is not a link >&2
                else
                    warn $target is pointing to  "$(readlink $target)" >&2
                    warn while it should point to "$(readlink -m $link )" >&2
                fi
                warn removing $link
                running mv $target ${target}-BACKUP
            else
                verbose $target do not exists >&1
            fi
            running cp -a $link $target
        else
            verbose $target is correctly pointing to "$(readlink -m $target )" is equal what $link is pointing to "$(readlink -m $link )"
        fi
    else
        echo $link is not a link, not doing anything >&2
    fi
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
            read _ UBUNTU_VERSIO``N_NAME <<< "$VERSION"
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

function setup_apt_upgrade_system()
{
    sudo apt -y clean
    sudo apt -y autoremove
    sudo apt -y autoclean
    sudo apt -y update
    sudo apt-file update
    sudo apt -y clean
    sudo apt -y autoremove
    sudo apt -y autoclean
    sudo apt -y upgrade
    sudo apt -y clean
    sudo apt -y autoremove
    sudo apt -y autoclean
    sudo apt -y clean
    sudo apt -y autoremove
    sudo apt -y autoclean
}

function setup_apt_packages()
{
    running setup_apt_repo

    running setup_apt_upgrade_system

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
        "DEB_PKG_MESSAGING" \
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
        "DEB_PKG_UTILS" \
        "DEB_PKG_MEDIA" \
        "DEB_PKG_WINDOW" \
        "DEB_PKG1_NET" \
        "DEB_PKG_DOC"  \
        "DEB_PKG_DOC_PUB"
    do
        if ! eval sudo apt -y install \$$pkg
        then
            for p in $(eval echo \$$pkg)
            do
                running sudo apt -y install ${p}
            done
        fi
    done

    for pkg in "$PY_PIP_PKG"
    do
        sudo pip install $pkg
    done
}

function setup_ecrypt_private()
{
    sudo apt -y install ecryptfs-utils

    if ! mount | grep $HOME/.Private
    then
        if [ ! -f ~/.ecryptfs/wrapped-passphrase ]
        then
	          ecryptfs-setup-private
        fi

        # # TODO BUG check for changes in homedir
        # # sed -i 's@/Private@/.Private@' ~/.ecryptfs/Private.mnt
        # echo $HOME/${RESOURCEPATH}/${USERORGMAIN}/readwrite/private/user/noenc/Private > ~/.ecryptfs/Private.mnt
        # ecryptfs-mount-private
    fi
    if [ ! -e ~/.ecryptfs -o -d ~/.ecryptfs ]
    then
        if [ ! -L ~/.ecryptfs ]
        then
            cp -ar ~/.ecryptfs ~/.ecryptfs-BAK
            setup_copy_link ~/.setup/.config/_home/.ecryptfs ~/.ecryptfs
            cp -f ~/.ecryptfs-BAK/Private.sig        ~/.ecryptfs/Private.sig
            cp -f ~/.ecryptfs-BAK/wrapped-passphrase ~/.ecryptfs/wrapped-passphrase
            cp -f ~/.ecryptfs-BAK/sedDxBKNi          ~/.ecryptfs/sedDxBKNi
        else
            setup_copy_link ~/.setup/.config/_home/.ecryptfs ~/.ecryptfs
        fi
    fi

    # TODO resolve migration of ~/.ecryptfs/Private.mnt
    # from $HOME/.Private to $HOME/${RESOURCEPATH}/${USERORGMAIN}/readwrite/private/user/noenc/Private
}

function setup_tmp_ssh_keys()
{
    SSH_KEY_ENC_DUMP=$2
    SSH_DIR=$1
    if ! ssh-add -l
    then
        if [ -f ~/.ssh/login-keys.d/github -a -f ~/.ssh/login-keys.d/github.pub ]
        then
            ssh-add ~/.ssh/login-keys.d/github
        fi
    fi                          # if ! ssh-add -l
    if ! ssh-add -l
    then
        sudo apt -y install openssl
        if [ "x$SSH_KEY_ENC_DUMP" != "x" -a -f "$SSH_KEY_ENC_DUMP" ]
        then
            ## bring the ssh keys
            if [ ! -r $SSH_DIR/nosecure.d/ssh/keys.d/github ]
            then
	              mkdir -p $SSH_DIR
                # TODO BUG not working
	              openssl enc -in "$SSH_KEY_ENC_DUMP" -aes-256-cbc -d | tar -zxvf - -C $SSH_DIR
            fi

            if ! ssh-add -l
            then
	              ssh-add $SSH_DIR/nosecure.d/ssh/keys.d/github
            fi
        else                    # if [ "x$SSH_KEY_ENC_DUMP" != "x" -a -f "$SSH_KEY_ENC_DUMP" ]
            echo setup_tmp_ssh_keys: key file not provided or not exists.
            exit -1
        fi
    fi                          # if ! ssh-add -l
}

function setup_ssh_keys()
{
    SSH_KEY_ENC_DUMP=$1

    local OSETUP_DIR=~/${RESOURCEPATH}/${USERORGMAIN}/readwrite/public/user/osetup
    ## bring the ssh keys
    if [ ! -r ${OSETUP_DIR}/nosecure.d/ssh/keys.d/github ]
    then
        if [ "x$SSH_KEY_ENC_DUMP" != "x" -a -f "$SSH_KEY_ENC_DUMP" ]
        then
            sudo apt -y install openssl

            SSH_KEY_ENC_DUMP=$1
            SSH_DIR=$2

            if ! mount | grep "$HOME/.Private"
            then
                running setup_ecrypt_private
                running /usr/bin/ecryptfs-mount-private
            fi

            if ! mount | grep "$USER/.Private"
            then
                if [ -d ${OSETUP_DIR} ]
                then
                    if [ -d ${OSETUP_DIR}/nosecure.d -a -L ${OSETUP_DIR}/secure -a -d ${OSETUP_DIR}/secure ]
                    then
                        if [ ! -e ${OSETUP_DIR}/nosecure.d/ssh/authorized_keys ]
                        then
                            touch ${OSETUP_DIR}/nosecure.d/ssh/authorized_keys
                        fi

                        if [ ! -e ${OSETUP_DIR}/secure/ssh/known_hosts ]
                        then
                            touch ${OSETUP_DIR}/secure/ssh/known_hosts
                        fi

                        if [ ! -e ${OSETUP_DIR}/secure/ssh/authorized_keys ]
                        then
                            touch ${OSETUP_DIR}/secure/ssh/authorized_keys
                        fi

                        openssl enc -in "$SSH_KEY_ENC_DUMP" -aes-256-cbc -d | tar -zxvf - -C ${OSETUP_DIR}/
                    else        # if [ -d ${OSETUP_DIR}/nosecure.d -a -L ${OSETUP_DIR}/secure -a -d ${OSETUP_DIR}/secure ]
                        echo setup_ssh_keys: directories ${OSETUP_DIR}${OSETUP_DIR}/nosecure.d or ${OSETUP_DIR}/secure not exists.
                    fi
                else            # if [ -d ${OSETUP_DIR} ]
                    echo setup_ssh_keys: directory ${OSETUP_DIR} not exists.
                fi
            else                # if ! mount | grep "$USER/.Private"
                echo setup_ssh_keys: "$USER/.Private" not mounted. >&2
            fi                  # if ! mount | grep "$USER/.Private"
        else                    # if [ "x$SSH_KEY_ENC_DUMP" != "x" -a -f "$SSH_KEY_ENC_DUMP" ]
            echo setup_ssh_keys: key file not provided or not exists.
        fi
    fi                          # if [ ! -r ${OSETUP_DIR}/nosecure.d/ssh/keys.d/github ]

    if ! ssh-add -l
    then
	      ssh-add ${OSETUP_DIR}/nosecure.d/ssh/keys.d/github
    fi
}

function setup_setup_dir()
{
    if [ ! -L ~/.setup ]
    then
	      rm -rf ~/.setup
    fi
    setup_make_link ${RESOURCEPATH}/${USERORGMAIN}/readwrite/public/user/rc ~/.setup
}

function setup_pi_dir()
{
    if [ ! -d ~/.pi -a -d ~/.setup/pi ]
    then
	      setup_make_link  .setup/pi ~/.pi
	      setup_make_link  ../${RESOURCEPATH}/${USERORGMAIN}/readwrite/private/user/orgp ~/.pi/org
    fi
}

function setup_emacs_dir()
{
    if [ ! -d ~/.emacs.d/.git ]
    then
	      if [ -d ~/.emacs.d ]
        then
            mv ~/.emacs.d ~/.emacs.d-old
        fi
	      setup_make_link ${RESOURCEPATH}/${USERORGMAIN}/readonly/public/user/spacemacs ~/.emacs.d
    fi
}

function setup_git_tree_repo()
{
    if [ $# -eq 2 ]
    then
        local GITURL=$1
        local GITDIR_BASE=$2

        mkdir -p "$(dirname ${GITDIR_BASE} )"
        if [ ! -d "${GITDIR_BASE}/" ]
        then
            running git -c core.sshCommand="$GIT_SSH_OPTION" clone --recursive  ${GITURL} ${GITDIR_BASE}
        else
            running git -c core.sshCommand="$GIT_SSH_OPTION" -C ${GITDIR_BASE} submodule foreach git -c core.sshCommand="$GIT_SSH_OPTION" status
            running git -c core.sshCommand="$GIT_SSH_OPTION" -C ${GITDIR_BASE} submodule foreach git -c core.sshCommand="$GIT_SSH_OPTION" pull --rebase
            # running git -c core.sshCommand="$GIT_SSH_OPTION" -C ${GITDIR_BASE} pull --rebase
            running git -c core.sshCommand="$GIT_SSH_OPTION" -C ${GITDIR_BASE} fetch
            running git -c core.sshCommand="$GIT_SSH_OPTION" -C ${GITDIR_BASE} status
        fi
    else
        echo setup_git_tree_repo: Not two args giturl gittreedir_base not provided. >&2
    fi
}

function setup_git_repos()
{
    # TODO [ISSUE] add code to handle upstream remote branch changes and merging to origin branch

    # RESOURCEPATH=".repos/git/main/resource"
    # USERORGMAIN="userorg/main"

    running setup_git_tree_repo git@github.com:sharad/userorg.git ~/${RESOURCEPATH}/userorg

    if false                    # decide through command line arguments
    then
        running setup_git_tree_repo git@bitbucket.org:sh4r4d/docorg.git ~/${RESOURCEPATH}/info/doc/orgs/private/doc

        running setup_git_tree_repo git@bitbucket.org:sh4r4d/mediaorg.git ~/${RESOURCEPATH}/data/multimedia/orgs/private/media/
    fi
}

function setup_config_dirs()
{
    running setup_ecrypt_private
    running setup_setup_dir
    running setup_pi_dir
    running setup_emacs_dir
}

function setup_user_config_setup()
{
    RCHOME="$HOME/${RESOURCEPATH}/${USERORGMAIN}/readwrite/public/user/rc/.config/_home/"
    if [ -d "${RCHOME}" ]
    then
	      if mkdir -p ~/_old_dot_filedirs
        then
	          # mv ~/.setup/.config/_home/.setup $TMPDIR/Xsetup
	          cd "${RCHOME}"
	          for c in .[a-zA-Z^.^..]* *
	          do
                echo considering $c
                clink=$(readlink $c)
	              if [ "$c" != ".repos" -a "$c" != ".setup" -a "$c" != ".gitignore" -a "$c" != "acyclicsymlinkfix" -a "$c" != "." -a "$c" != ".." -a "$clink" != ".." ] # very important
	              then
                    # setup_copy_link $c ~/$c
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

                        else    # if [ ! -L ~/$c -o "$(readlink ~/$c)" != "$(readlink $c)" ]
                            echo not doing anything $c ~/$c
                        fi      # if [ ! -L ~/$c -o "$(readlink ~/$c)" != "$(readlink $c)" ]
                    else        # if [ -e ~/$c ]
                        running cp -af $c ~/$c
                        echo done setting up $c
		                fi          # if [ -e ~/$c ]
                else            # if [ "$c" != ".repos" -a "$c" != ".setup" -a "$c" != ".gitignore" -a "$c" != "acyclicsymlinkfix" -a "$c" != "." -a "$c" != ".." -a "$clink" != ".." ] # very important
                    echo not setting up $c
	              fi              # if [ "$c" != ".repos" -a "$c" != ".setup" -a "$c" != ".gitignore" -a "$c" != "acyclicsymlinkfix" -a "$c" != "." -a "$c" != ".." -a "$clink" != ".." ] # very important
	          done
	          # mv $TMPDIR/Xsetup ~/.setup/.config/_home/.setup
	          cd -
        fi                      # if mkdir -p ~/_old_dot_filedirs
        rmdir ~/_old_dot_filedirs
    else                        # if [ -d "${RCHOME}" ]
        echo "${RCHOME}" not exists >&2
    fi                          # if [ -d "${RCHOME}" ]

    # if false
    # then
    #     if [ -d ~/.setup/.config/_home/acyclicsymlinkfix ]
    #     then
    #         for symlnk in ~/.setup/.config/_home/acyclicsymlinkfix/.Volumes \
    #                       ~/.setup/.config/_home/acyclicsymlinkfix/Desktop \
    #                       ~/.setup/.config/_home/acyclicsymlinkfix/Downloads \
    #                       ~/.setup/.config/_home/acyclicsymlinkfix/Music \
    #                       ~/.setup/.config/_home/acyclicsymlinkfix/Pictures
    #         do
    #             cp -a $symlnk ~/
    #         done
    #     fi
    # fi
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
    setup_copy_link ~/.setup/.config/_home/Documents ~/Documents
}

function setup_public_html()    # TODO
{
    setup_copy_link ~/.setup/.config/_home/public_html ~/public_html
}

function setup_mail()
{
    local SYSTEM_DIR=~/${RESOURCEPATH}/${USERORGMAIN}/readwrite/public/system/system
    sudo apt -y install dovecot-core dovecot-imapd ntpdate postfix
    if [ -d ${SYSTEM_DIR}/ubuntu/etc/postfix ]
    then
        if [ ! -d /etc/postfix-ORG ]
        then
            sudo cp -ar /etc/postfix /etc/postfix-ORG
            for f in ${SYSTEM_DIR}/ubuntu/etc/postfix/*
            do
                b=$(basename $f)
                cp $f /etc/postfix/
            done
        fi

        if [ ! -d /etc/dovecot-ORG ]
        then
            sudo cp -ar /etc/dovecot /etc/dovecot-ORG
            sudo cp ${SYSTEM_DIR}/ubuntu/etc/dovecot/conf.d/10-mail.conf /etc/dovecot/conf.d/10-mail.conf
        fi
    else
        echo ${SYSTEM_DIR}/ubuntu/etc/postfix not exists >&2
    fi
}

function setup_crontab()
{
    m4 ~/.setup/crontab.m4 2>/dev/null | tee ~/.setup/crontab | crontab
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

###{{{ libs
function setup_mvc_dirs()
{
    if [ $# -eq 1 ]
    then
        containerdir="$1"

        mkdir -p ${containerdir}/model.d
        if [ -d ${containerdir}/model.d ] && ls ${containerdir}/model.d/*
        then
            modelsymlink=0
            for sdir in ${containerdir}/model.d/*
            do
                if [ -L "$sdir" ]
                then
                    modelsymlink=1
                fi
                sdirbase=$(basename "$sdir")
                setup_make_link ../model.d/${sdirbase} ${containerdir}/control.d/${sdirbase}
            done
            if [ "$modelsymlink" -eq 0 ]
            then
                error No symlink for model dirs exists in ${containerdir}/model.d create it.
            fi
        fi              # if [ -d ${containerdir}/model.d ]

        mkdir -p ${containerdir}/control.d
        if [ -d ${containerdir}/control.d ] && ls ${containerdir}/control.d/*
        then
            modelsymlink=0
            for sdir in ${containerdir}/control.d/*
            do
                if [ -L "$sdir" ]
                then
                    modelsymlink=1
                fi
                sdirbase=$(basename "$sdir")
                setup_make_link ../control.d/${sdirbase} ${containerdir}/view.d/${sdirbase}
            done
            if [ "$modelsymlink" -eq 0 ]
            then
                error No symlink for control dirs exists in ${containerdir}/control.d create it.
            fi
        fi              # if [ -d ${containerdir}/control.d ]
    else
        error one dir argument is require, but provided $# "$@"
    fi
}
###{{{ libs

function setup_machine_dir()
{
    # use namei to track
    # running setup_paradise

    local OSETUP_DIR=~/${RESOURCEPATH}/${USERORGMAIN}/readwrite/public/user/osetup
    local LOCALDIRS_DIR=~/${RESOURCEPATH}/${USERORGMAIN}/readwrite/public/user/localdirs


    setup_copy_link ~/.setup/.config/_home/.dirs.d ~/.dirs.d
    setup_copy_link ~/.setup/.config/_home/.fa     ~/.fa

    # ~/.osetup ~/.localdirs going to be removed.
    # can not use ~/.fa as it is for interactive usage and management.
    for l in ${OSETUP_DIR}/dirs.d/model.d/*/*
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

    # check local home model.d directory
    if [ -d ${LOCALDIRS_DIR} -a -d ${LOCALDIRS_DIR}/deps.d/model.d/machine.d ]
    then
        if [ ! -d ${LOCALDIRS_DIR}/deps.d/model.d/machine.d/$HOST ]
        then
            mkdir -p ${LOCALDIRS_DIR}/deps.d/model.d/machine.d/$HOST
            if [ -d ${LOCALDIRS_DIR}/deps.d/model.d/machine.d/$HOST ]
            then
                running  cp -ar ${LOCALDIRS_DIR}/deps.d/model.d/machine.d/sample ${LOCALDIRS_DIR}/deps.d/model.d/machine.d/$HOST
                echo add ${LOCALDIRS_DIR}/deps.d/model.d/machine.d/$HOST into git
            fi
        fi                      # if [ ! -d ${LOCALDIRS_DIR}/deps.d/model.d/machine.d/$HOST ]
    fi                          # if [ -d ${LOCALDIRS_DIR} -a -d ${LOCALDIRS_DIR}/deps.d/model.d/machine.d ]

    if [ -d ${LOCALDIRS_DIR}/deps.d/model.d/machine.d/$HOST ]
    then
       running setup_make_link $HOST ${LOCALDIRS_DIR}/deps.d/model.d/machine.d/default
    fi
}

###{{{ libs
# worker
function setup_dep_control_storage_class_dirs()
{
    if [ $# -eq 4 ]
    then
        local storageclass="$1"
        local class="$2"
        local classinstdir="$3"

        local position=${4}


        local classcontainer=$(basename $class)
        local classpath=$(dirname $class)
        if [ ${classpath} = "." ]
        then
            classpath=
        fi




        local classpatharray=( ${classpath//\// } )
        local classlen=${#classpatharray[@]}
        local updirslenspace=$(printf "%${classlen}s")

        local updirs=${updirslenspace// /"../"}



        local LOCALDIRS_DIR=~/${RESOURCEPATH}/${USERORGMAIN}/readwrite/public/user/localdirs
        local machinedir=${LOCALDIRS_DIR}/deps.d/model.d/machine.d
        local hostdir=${machinedir}/$HOST

        local classmodeldir=${hostdir}/volumes.d/control.d/${classpath}${classpath:+/}${storageclass}/${classcontainer}.d

        local fullupdirs=${updirs}../../../..

        mkdir -p $classmodeldir/model.d
        mkdir -p $classmodeldir/control.d
        mkdir -p $classmodeldir/view.d

        if [ -d ${hostdir}/volumes.d/model.d ] && ls ${hostdir}/volumes.d/model.d/*
        then
            modelsymlink=0
            for mdir in ${hostdir}/volumes.d/model.d/*
            do
                if [ -L "$mdir" ]
                then
                    modelsymlink=1
                fi

                mdirbase=$(basename "$mdir")
                volclasspathinstdir="model.d/${mdirbase}/${classpath}${classpath:+/}${classinstdir}"

                running mkdir -p ${hostdir}/volumes.d/${volclasspathinstdir}

                info updirs=$updirs
                running setup_make_link ${fullupdirs}/${volclasspathinstdir} $classmodeldir/model.d/${mdirbase}
            done

            if [ "$modelsymlink" -eq 0 ]
            then
                error No symlink for model volume dirs exists in ${hostdir}/volumes.d/model.d create it.
            fi
        fi              # if [ -d ${hostdir}/volumes.d/model.d ]
        running setup_mvc_dirs ${classmodeldir}/
    else
        error setup_dep_control_storage_class_dirs Not correct number of arguments.
    fi
}

function setup_deps_control_class_dirs()
{
    # use namei to track

    # ls ~/.fa/localdirs/deps.d/model.d/machine.d/default/volumes.d/model.d/*/
    # ls ~/fa/localdirs/deps.d/model.d/machine.d/$HOST/${class}.d/

    if [ $# -eq 3 ]
    then
        local storageclass="$1"
        local class="$2"
        local classinstdir="$3"

        local classcontainer=$(basename $class)
        local classpath=$(dirname $class)
        if [ ${classpath} = "." ]
        then
            classpath=
        fi




        local classpatharray=( ${classpath//\// } )
        local classlen=${#classpatharray[@]}
        local updirslenspace=$(printf "%${classlen}s")

        local updirs=${updirslenspace// /"../"}



        local LOCALDIRS_DIR=~/${RESOURCEPATH}/${USERORGMAIN}/readwrite/public/user/localdirs
        local machinedir=${LOCALDIRS_DIR}/deps.d/model.d/machine.d
        local hostdir=${machinedir}/$HOST


        # check local home model.d directory
        if [ -d ${LOCALDIRS_DIR} -a -d ${machinedir} ]
        then
            if [ -d ${hostdir} ]
            then
                mkdir -p ${hostdir}

                running setup_make_link $HOST ${machinedir}/default

                # local classmodeldir=${hostdir}/volumes.d/control.d/${classpath}${classpath:+/}${storageclass}/${classcontainer}.d
                # local fullupdirs=${updirs}../../../..

                # mkdir -p $classmodeldir/model.d
                # mkdir -p $classmodeldir/control.d
                # mkdir -p $classmodeldir/view.d

                # if [ -d ${hostdir}/volumes.d/model.d ] && ls ${hostdir}/volumes.d/model.d/*
                # then
                #     modelsymlink=0
                #     for mdir in ${hostdir}/volumes.d/model.d/*
                #     do
                #         if [ -L "$mdir" ]
                #         then
                #             modelsymlink=1
                #         fi

                #         mdirbase=$(basename "$mdir")
                #         volclasspathinstdir="model.d/${mdirbase}/${classpath}${classpath:+/}${classinstdir}"

                #         running mkdir -p ${hostdir}/volumes.d/${volclasspathinstdir}

                #         info updirs=$updirs
                #         running setup_make_link ${fullupdirs}/${volclasspathinstdir} $classmodeldir/model.d/${mdirbase}
                #     done

                #     if [ "$modelsymlink" -eq 0 ]
                #     then
                #         error No symlink for model volume dirs exists in ${hostdir}/volumes.d/model.d create it.
                #     fi
                # fi              # if [ -d ${hostdir}/volumes.d/model.d ]
                # running setup_mvc_dirs ${classmodeldir}/

                running setup_dep_control_storage_class_dirs "$storageclass" "$class" "$classinstdir" 1

            else                # if [ -d ${hostdir} ]
                echo Please prepare ${hostdir} for your machine >&2
                exit -1
            fi                  # if [ -d ${hostdir} ]

        else                    # if [ -d ${LOCALDIRS_DIR} -a -d ${machinedir} ]
            echo ${LOCALDIRS_DIR} or ${machinedir} not exists. >&2
        fi                      # if [ -d ${LOCALDIRS_DIR} -a -d ${machinedir} ]
    else
        error setup_deps_control_class_dirs: Not correct number of arguments.
    fi                          # if [ $# -eq 2 ]
}

function setup_deps_control_data_sysdata_dirs()
{
    storageclassname="${1-local}"

    running setup_deps_model_volumes_dirs

    running setup_deps_control_class_dirs "$storageclassname" ${dataclassname}/sysdatas sysdata
}
function setup_deps_control_data_scratches_dirs()
{
    storageclassname="${1-local}"

    running setup_deps_model_volumes_dirs
    running setup_deps_control_class_dirs "$storageclassname" ${dataclassname}/scratches scratch
}
function setup_deps_control_data_main_dirs()
{
    storageclassname="${1-local}"

    running setup_deps_model_volumes_dirs
    running setup_deps_control_class_dirs "$storageclassname" ${dataclassname}/main main
}
function setup_deps_control_data_dirs()
{
    local storageclassname="${1-local}"

    running setup_deps_control_data_sysdata_dirs   "$storageclassname"
    running setup_deps_control_data_scratches_dirs "$storageclassname"
    running setup_deps_control_data_main_dirs      "$storageclassname"
}




function setup_deps_control_home_Downloads_dirs()
{
    local storageclassname="${1-local}"

    running setup_deps_model_volumes_dirs
    running setup_deps_control_class_dirs "$storageclassname" ${homeclassname}/Downloads Downloads
}
function setup_deps_control_home_dirs()
{
    local storageclassname="${1-local}"

    setup_deps_control_home_Downloads_dirs "$storageclassname"
}
###}}}

function setup_deps_model_storage_volumes_dir()
{
    local storageclassname=${1-local}
    local storageclassdirpath=/srv/volumes/$storageclassname/

    local LOCALDIRS_DIR=~/${RESOURCEPATH}/${USERORGMAIN}/readwrite/public/user/localdirs

    if [ -d ${LOCALDIRS_DIR}/deps.d/model.d/machine.d/$HOST/volumes.d/model.d -a -d $storageclassdirpath ]
    then
        modelsymlink_present=0
        for vgd in ${storageclassdirpath}/*
        do
            modelsymlink_present=1
            for vld in ${vgd}/*
            do
                local _location=$vld/users/$USER
                if [ -f $_location ]
                then
                    sudo mkdir -p $_location
                    sudo chown root.root $_location
                fi
                setup_make_link $_location "${LOCALDIRS_DIR}/deps.d/model.d/machine.d/$HOST/volumes.d/model.d/$storageclassname/$(basename $vgd)-$(basename $vld)"
            done
        done

        if [ "$modelsymlink_present" -eq 0 ]
        then
            error No disk partition mount are present in $storageclassdirpath create them. >&2
        fi
    fi       # if [ -d ${LOCALDIRS_DIR}/deps.d/model.d/machine.d/$HOST/volumes.d/model.d -a -d /srv/volumes/local ]
}

function setup_deps_model_volumes_dirs()
{
    local storageclassname="${1-local}"

    running setup_machine_dir

    # use namei to track
    local LOCALDIRS_DIR=~/${RESOURCEPATH}/${USERORGMAIN}/readwrite/public/user/localdirs
    # check local home model.d directory
    if [ -d ${LOCALDIRS_DIR} -a -d ${LOCALDIRS_DIR}/deps.d/model.d/machine.d ]
    then

        # running setup_machine_dir

        if [ -d ${LOCALDIRS_DIR}/deps.d/model.d/machine.d/$HOST ]
        then
            mkdir -p ${LOCALDIRS_DIR}/deps.d/model.d/machine.d/$HOST

            setup_make_link $HOST ${LOCALDIRS_DIR}/deps.d/model.d/machine.d/default

            setup_make_link "../../../../../../../../../../../../../../"  ${LOCALDIRS_DIR}/deps.d/model.d/machine.d/$HOST/home

            mkdir -p ${LOCALDIRS_DIR}/deps.d/model.d/machine.d/$HOST/volumes.d/model.d

            setup_deps_model_storage_volumes_dir "$storageclassname"

        else                    # if [ -d ${LOCALDIRS_DIR}/deps.d/model.d/machine.d/$HOST ]
            error Please prepare ${LOCALDIRS_DIR}/deps.d/model.d/machine.d/$HOST for your machine >&2
            exit -1
        fi                      # if [ -d ${LOCALDIRS_DIR}/deps.d/model.d/machine.d/$HOST ]
    fi                          # if [ -d ${LOCALDIRS_DIR} -a -d ${LOCALDIRS_DIR}/deps.d/model.d/machine.d ]
}

function setup_deps_control_volumes_dirs()
{
    # TODO?
    local storageclassname="${1-local}"
    position=${2-2}

    local sysdataname=sysdata

    # local sysdatascontinername=${dataclassname}/${sysdataname}s
    local sysdatascontinername=${dataclassname}/$storageclassname/${sysdataname}s

    local sysdatasdirname=${sysdatascontinername}.d
    # logicaldirs=(config deletable longterm preserved shortterm maildata)

    local LOCALDIRS_DIR=~/${RESOURCEPATH}/${USERORGMAIN}/readwrite/public/user/localdirs
    local machinedir=${LOCALDIRS_DIR}/deps.d/model.d/machine.d
    local hostdir=${machinedir}/$HOST
    local volumedir=${hostdir}/volumes.d

    running setup_deps_control_data_sysdata_dirs "$storageclassname"
    running setup_deps_control_class_dirs "$storageclassname" $sysdatascontinername $sysdataname

    for cdir in ${logicaldirs[*]} # config deletable longterm preserved shortterm maildata
    do
        # echo "${volumedir}/${viewdirname}/$cdir"

        if [ ! -L "${volumedir}/${viewdirname}/$cdir" -o ! -d "${volumedir}/${viewdirname}/$cdir" ]
        then

            for sysdatadir in ${volumedir}/control.d/${sysdatasdirname}/view.d/*
            do
                # TODO?
                volsysdatadirbase=$(basename ${sysdatadir})
                mkdir -p  ${volumedir}/control.d/${sysdatasdirname}/view.d/${volsysdatadirbase}/$cdir
            done
        fi
    done

    running running setup_deps_control_data_dirs "$storageclassname"
    running running setup_deps_control_home_dirs "$storageclassname"
}

function setup_deps_view_volumes_dirs()
{
    local storageclassname="${1-local}"
    local position=${2-2}

    # TODO?
    # ls ~/.fa/localdirs/deps.d/model.d/machine.d/default/volumes.d/control.d/

    # logicaldirs=(config deletable longterm preserved shortterm maildata)

    # use namei to track
    local LOCALDIRS_DIR=~/${RESOURCEPATH}/${USERORGMAIN}/readwrite/public/user/localdirs

    local machinedir=${LOCALDIRS_DIR}/deps.d/model.d/machine.d

    local hostdir=${machinedir}/$HOST

    local volumedir=${hostdir}/volumes.d

    local sysdataname=sysdata
    local sysdatascontinername=${dataclassname}/$storageclassname/${sysdataname}s
    local sysdatasdirname=${sysdatascontinername}.d
    local viewdirname=view.d

    # check local home model.d directory
    if [ -d ${LOCALDIRS_DIR} -a -d ${machinedir} ]
    then
        if [ -d ${hostdir} ]
        then

            mkdir -p ${hostdir}

            setup_make_link $HOST ${machinedir}/default

            if [ -d ${volumedir}/model.d ]
            then
                modelsymlink=0
                for mdir in ${volumedir}/model.d/*
                do
                    if [ -L "$mdir" ]
                    then
                        modelsymlink=1
                    fi
                done

                if [ "$modelsymlink" -eq 0 ]
                then
                    error No symlink for model dirs exists in ${volumedir}/model.d create it. >&2
                fi
            else
                error ${volumedir}/model.d not exists.
            fi                  # if [ -d ${volumedir}/model.d ]


            # running setup_deps_control_data_sysdata_dirs
            # running setup_deps_control_class_dirs "$storageclassname" $sysdatascontinername $sysdataname
            running setup_deps_control_volumes_dirs "$storageclassname"
            running setup_deps_control_class_dirs "$storageclassname" $sysdatascontinername $sysdataname

            mkdir -p ${volumedir}/${viewdirname}
            for cdir in ${logicaldirs[*]} # config deletable longterm preserved shortterm maildata
            do
                # echo "${volumedir}/${viewdirname}/$cdir"

                if [ ! -L "${volumedir}/${viewdirname}/$cdir" -o ! -d "${volumedir}/${viewdirname}/$cdir" ]
                then
                    if [ ! -L "${volumedir}/${viewdirname}/$cdir" ]
                    then
                        error No symlink exists for "${volumedir}/${viewdirname}/$cdir", prepare it.
                    fi
                    if [ ! -d "${volumedir}/${viewdirname}/$cdir" ]
                    then
                        error No target directory $(readlink -m $cdir) exist for symlink "${volumedir}/${viewdirname}/$cdir", create it.
                    fi

                    for sysdatadir in ${volumedir}/control.d/${sysdatasdirname}/view.d/*
                    do
                        volsysdatadirbase=$(basename ${sysdatadir})
                        info ln -s ../control.d/${sysdatasdirname}/view.d/${volsysdatadirbase}/$cdir "${volumedir}/${viewdirname}/$cdir"
                    done
                fi
            done


        else                    # if [ -d ${hostdir} ]
            error Please prepare ${hostdir} for your machine >&2
            exit -1
        fi                      # if [ -d ${hostdir} ]
    else
        error ${LOCALDIRS_DIR} or ${machinedir} not exists
        exit -1
    fi                          # if [ -d ${LOCALDIRS_DIR} -a -d ${machinedir} ]
}                               # function setup_deps_view_volumes_dirs()

function setup_deps_dirs()
{
    local storageclassname="${1-local}"

    running setup_deps_model_volumes_dirs "$storageclassname"

    running setup_deps_control_data_dirs "$storageclassname"
    running setup_deps_control_home_dirs "$storageclassname"

    running setup_deps_control_volumes_dirs "$storageclassname"
    running setup_deps_view_volumes_dirs "$storageclassname"
}

function setup_resource_view_dirs()
{
    local LOCALDIRS_DIR=~/${RESOURCEPATH}/${USERORGMAIN}/readwrite/public/user/localdirs
    local resourcedir=${LOCALDIRS_DIR}
    local resource_viewdir=${resourcedir}/view.d
    local resource_controldir=${resourcedir}/control.d
}

function setup_resource_control_dirs()
{
    local LOCALDIRS_DIR=~/${RESOURCEPATH}/${USERORGMAIN}/readwrite/public/user/localdirs
    local resourcedir=${LOCALDIRS_DIR}
    local resource_viewdir=${resourcedir}/view.d
    local resource_controldir=${resourcedir}/control.d
}

function setup_resource_model_dirs()
{
    local LOCALDIRS_DIR=~/${RESOURCEPATH}/${USERORGMAIN}/readwrite/public/user/localdirs
    local resourcedir=${LOCALDIRS_DIR}
    local resource_viewdir=${resourcedir}/view.d
    local resource_controldir=${resourcedir}/control.d
}

function setup_resource_view_volumes_logical_dirs()
{
    local LOCALDIRS_DIR=~/${RESOURCEPATH}/${USERORGMAIN}/readwrite/public/user/localdirs
    local resourcedir=${LOCALDIRS_DIR}
    local resource_viewdir=${resourcedir}/view.d
    local resource_controldir=${resourcedir}/control.d
}

function setup_resource_dirs()
{
    running setup_resource_model_dirs
    running setup_resource_control_dirs
    running setup_resource_view_dirs
    running setup_resource_view_volumes_logical_dirs
}

function setup_osetup_cache_dirs()
{
    # ls -l /home/s/hell/.repos/git/main/resource/userorg/main/readwrite/public/user/osetup/dirs.d/control.d/cache.d/
    # mkdir -p /home/s/hell/.repos/git/main/resource/userorg/main/readwrite/public/user/osetup/dirs.d/model.d/volume.d/*/cache.d
    :
}

function setup_osetup_dirs()
{
    running setup_osetup_cache_dirs
}

function setup_dirs()
{
    running setup_machine_dir
    running setup_deps_dirs
    running setup_resource_dirs
    running setup_osetup_dirs
    running setup_Documentation
    running setup_public_html
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
    local SYSTEM_DIR=~/${RESOURCEPATH}/${USERORGMAIN}/readwrite/public/system/system
    if [ -r /etc/apache2/apache2.conf ]
    then
        if [ ! -d /usr/local/etc/apache ]
        then
            mkdir -p /usr/local/etc/
            cp -r ${SYSTEM_DIR}/ubuntu/usr/local/etc/apache /usr/local/etc/apache
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
        fi                      # if ! grep /usr/local/etc/apache /etc/apache2/apache2.conf
    fi                          # if [ -r /etc/apache2/apache2.conf ]
}

function setup_clib_installer()
{
    sudo apt-get -y install libcurl4-gnutls-dev -qq
    if [ ! -d /usr/local/stow/clib/ ]
    then
        if running git -c core.sshCommand="$GIT_SSH_OPTION" clone https://github.com/clibs/clib.git $TMPDIR/clib
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
    install_bpkg_pkg sharad/gitwatch
}

function set_window_share()
{
    # //WIN7-SPRATAP/Users/spratap/Desktop/Desktop/Docs  /media/winshare cifs credentials=/media/.credentials,uid=nobody,iocharset=utf8,noperm 0 0
    echo
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
    echo running "$@"
    local _cmd=$1
    shift
    if [ ! $noaction ]
    then
        $_cmd "$@"
    fi
}

function error() {
    notify "$*"  >&2
    logger "$*"
}

function warn() {
    if [ $warn ] ; then
        notify "$*" >&2
    fi
    logger "$*"
}

function verbose() {
    if [ $verbose ] ; then
        notify "$*" >&2
    fi
    logger "$*"
}

function info() {
    notify "$*" >&2
    logger "$*"
}

function notify() {
    echo "${pgm}:" "$*"

    if [ -t 1 ]
    then
        echo "${pgm}:" "$*"
    else
        echo "${pgm}:" "$*"
        notify-send "${pgm}:" "$*"
    fi
}

function logger() {
    #creating prolem
    command logger -p local1.notice -t ${pgm} -i - $USER : "$*"
}

pgm=$(basename $0)

main

exit
