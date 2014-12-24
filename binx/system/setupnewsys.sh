#!/bin/zsh




# do apt-clone

if [ ! -d ~/.pi/org ] ; then
    mkdir -p ~/.pi/org
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

if [ ! -d ~/.osetup-trunk ] ; then
    git clone ssh://git@bitbucket.org/sh4r4d/osetup.git ~/osetup-trunk
    cd ~/; ln -s .osetup-trunk .osetup
    cp -d ~/.osetup/_home/.* ~/
else
    print ~/.osetup-trunk already present
fi

print Installing sbcl, stow
sudo apt-get install sbcl stow

foreach d (/usr/local/stow /usr/local/builds) {
    mkdir -p $d
    sudo chown ${USER}:${GID} $d

}

git clone gitolite@lispm:s/lisp/stumpwm.git /usr/local/builds/stumpwm

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

# check for /atlantic and pacific dirs
# copy ~/.config/common-lisp
# copy .osetup
# setup .ssh/login-keys.d
# setup quicklisp
# run sbcl
# setup ecryptfs-setup-private
