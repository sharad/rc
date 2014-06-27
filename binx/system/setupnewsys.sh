#/usr/bin/env zsh



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
