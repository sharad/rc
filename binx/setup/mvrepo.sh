#!/usr/bin/env bash

exit

OLDREPO=~/.repos/git/user
NEWREPO=~/.repos/git/main/resource/userorg
NEWDOCORG=~/.repos/git/main/resource/info/doc/

cp -a $OLDREPO/spacemacs/{elpa,.cache,autoconfig} $NEWREPO/main/readonly/public/user/spacemacs
cp -a  $OLDREPO/rc/xemacs/elpa/upload $NEWREPO/main/readwrite/public/user/rc/xemacs/elpa/upload
cp -a  $OLDREPO/rc/ssh/config $NEWREPO/main/readwrite/public/user/rc/rc/ssh/config
mkdir -p $NEWREPO/main/readwrite/public/user/rc/rc/ssh/tmp
cp $OLDREPO/rc/notmuch-config $NEWREPO/main/readwrite/public/user/rc/
cp $OLDREPO/rc/offlineimaprc $NEWREPO/main/readwrite/public/user/rc/offlineimaprc
# make sure ~/.offlineimap link point to correct dir.




rm -f ~/.localdirs ;  cp -a  main/readwrite/public/user/rc/_home/.localdirs ~/.localdirs

cd $OLDREPO/osetup;
for f in  setup.d/rsetup.d/*/*/*
do
    mkdir -p $(dirname $NEWREPO/main/readwrite/public/user/osetup/$f)
    cp -a $f $NEWREPO/main/readwrite/public/user/osetup/$f
done
cd -

cp  $OLDREPO/osetup/osetup/nosecure.d/ssh/authorized_keys  $NEWREPO/main/readwrite/public/user/osetup/nosecure.d/ssh/
cp  $OLDREPO/osetup/osetup/nosecure.d/ssh/keys.d/{internet,fortinet,github,work}  $NEWREPO/main/readwrite/public/user/osetup/nosecure.d/ssh/keys.d/
cp $OLDREPO/osetup/data.d/emacs.d/gnus.d/message.d/signatures.d/* $NEWREPO/main/readwrite/public/user/osetup/data.d/emacs.d/gnus.d/message.d/signatures.d

mkdir -p $NEWDOCORG/orgs/ftnt/doc/contents
ln -s ../../orgs/ftnt/contents/org  $NEWREPO/main/readwrite/public/user/doc/CreatedContent/contents/virtual/org/ftnt
ln -s ../../orgs/ftnt/contents/muse  $NEWREPO/main/readwrite/public/user/doc/CreatedContent/contents/virtual/muse/ftnt
ln -s ../../orgs/ftnt/contents/misc  $NEWREPO/main/readwrite/public/user/doc/CreatedContent/contents/virtual/misc/ftnt

if mkdir ~/.backup
then
    for d in .dirs.d .setup .LocalDirs.d .localdirs Documents
    do
        cp -a ~/$d ~/.backup
        rm -f ~/$d
        cp -a $NEWREPO/main/readwrite/public/user/rc/_home/$d ~/
    done
fi


if mkdir -p ~/.backup/localdirs/home.d
then
    cd $OLDREPO/localdirs/home.d
    for d in *
    do
        cp -a $d $NEWREPO/main/readwrite/public/user/localdirs/home.d/
    done
fi

cp -ar $NEWREPO/main/readwrite/public/user/localdirs/deps.d/model.d/machine.d/lispm  $NEWREPO/main/readwrite/public/user/localdirs/deps.d/model.d/machine.d/$HOST
ln -s $HOST $NEWREPO/main/readwrite/public/user/localdirs/deps.d/model.d/machine.d/default


cp -ra $OLDREPO/doc/CreatedContent/*  $NEWDOCORG/orgs/ftnt/doc/

ll $NEWREPO/main/readwrite/public/user/doc/CreatedContent/contents/virtual/misc/default
ll $NEWREPO/main/readwrite/public/user/doc/CreatedContent/contents/virtual/muse/default
ll $NEWREPO/main/readwrite/public/user/doc/CreatedContent/contents/virtual/org/default
