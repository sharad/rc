#!/usr/bin/env zsh

# do apt-clone

# check for /atlantic and pacific dirs
foreach d (/all/reiser /all/res/share) {
    if [ -d $d ] ; then
        print $d is presnt
    else
        print $d is not presnt, exiting
        exit -1
    fi
}

#setup gitolite

print Installing sbcl, stow
sudo apt-get install sbcl stow

if [ ! -d ~/.pi/org ] ; then
    mkdir -p ~/.pi/
    git clone ssh://git@bitbucket.org/sh4r4d/orgp.git ~/.pi/org
else
    print ~/.pi/org already present
fi

if [ ! -d ~/.setup-trunk ] ; then
    git clone git@github.com:sharad/rc.git ~/.setup-trunk
    cd ~/; ln -s .setup-trunk .setup
    cp -d ~/.setup/_home/.* ~/
else
    print ~/.setup-trunk already present
fi



foreach d (  osetup system sysinfo ) {
if [ ! -d  ] ; then
    git clone git@github.com:sharad/$d.git ~/.$d-trunk
    cd ~/;
    ln -s .$d-trunk .$d
    cp -d ~/.setup/_home/.* ~/
    cd -
else
    print ~/.$d-trunk already present
fi
}


sudo mkdir -p /usr/local/stow
foreach d ( /usr/local/builds ) {
    mkdir -p $d
    sudo chown ${USER}:${GID} $d

}

# copy ~/.config/common-lisp
#depend .setup
mkdir -p ~/.config/
cp -r ~/.setup/config/common-lisp ~/.config/

# setup ecryptfs-setup-private
if [ -d ~/.Private ] ; then
    print ~/.Private already present
else
    ecryptfs-setup-private
    echo $HOME/.Private > ~/.ecryptfs/Private.mnt
fi

# setup .ssh/login-keys.d
#depend .osetup .setup .Private
mkdir ~/.ssh
ln -s .setup/ssh/login-keys.d ~/.ssh/login-keys.d
ls -l ~/.ssh/login-keys.d


# setup quicklisp
# depend sbcl asdf
cd /tmp
curl -O http://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp  --eval '(progn (quicklisp-quickstart:install)(quit))'
sudo mkdir /all/res/share/common-lisp/
sudo mv  ~/quicklisp /all/res/share/common-lisp/
cd -


git clone gitolite@lispm:s/lisp/stumpwm.git /usr/local/builds/stumpwm
git -C /usr/local/builds/stumpwm checkout pa-point-timeout

cd /usr/local/builds/stumpwm/
sudo rm -f   /usr/local/stow/stumpwm-lispm/bin/stumpwm
ls /usr/local/stow/stumpwm-lispm/bin/stumpwm
/usr/local/builds/stumpwm/autogen.sh
/usr/local/builds/stumpwm/configure --prefix /usr/local/stow/stumpwm-lispm --with-lisp=sbcl
make clean all -C /usr/local/builds/stumpwm
sed -i s/install: stumpwm.info stumpwm/install: stumpwm/g   /usr/local/builds/stumpwm/Makefile
sudo make install -C /usr/local/builds/stumpwm
md5sum /usr/local/stow/stumpwm-lispm/bin/stumpwm /usr/local/builds/stumpwm/stumpwm
make clean
ls /usr/local/stow/stumpwm-lispm/bin/stumpwm
cd -

# copy .osetup
# run sbcl
# setup ls .osetup/dirs.d especially reiser-disk and scratch.d/
# setup mail
# setup ~/.maildir folder
# postfix and dovecot
# compile gnus
# setup .authinfo
# setup ecryptfs password
