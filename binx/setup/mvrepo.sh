#!/bin/bash

exit


cp -a  ~/.emacs.d/elpa ~/.emacs.d/.cache ~/.emacs.d/autoconfig ~/.repos/git/main/resource/userorg/main/readonly/public/user/spacemacs
rm -f ~/.localdirs ;  cp -a  main/readwrite/public/user/rc/_home/.localdirs ~/.localdirs

cd ~/.osetup;
for f in  setup.d/rsetup.d/*/*/*
do
    mkdir -p $(dirname ~/.repos/git/main/resource/userorg/main/readwrite/public/user/osetup/$f)
    cp -a $f ~/.repos/git/main/resource/userorg/main/readwrite/public/user/osetup/$f
done
cd -

cp  ~/.osetup/nosecure.d/ssh/authorized_keys  ~/.repos/git/main/resource/userorg/main/readwrite/public/user/osetup/nosecure.d/ssh/
cp  ~/.osetup/nosecure.d/ssh/keys.d/{internet,fortinet,github,work}  ~/.repos/git/main/resource/userorg/main/readwrite/public/user/osetup/nosecure.d/ssh/keys.d/

mkdir -p ~/.repos/git/main/resource/info/doc/orgs/private/doc/contents
mkdir -p ~/.repos/git/main/resource/info/doc/orgs/ftnt/doc/contents
ln -s ../../orgs/ftnt/contents/org  ~/.repos/git/main/resource/userorg/main/readwrite/public/user/doc/CreatedContent/contents/virtual/org/ftnt
ln -s ../../orgs/ftnt/contents/muse  ~/.repos/git/main/resource/userorg/main/readwrite/public/user/doc/CreatedContent/contents/virtual/muse/ftnt
ln -s ../../orgs/ftnt/contents/misc  ~/.repos/git/main/resource/userorg/main/readwrite/public/user/doc/CreatedContent/contents/virtual/misc/ftnt

if mkdir ~/.backup
then
    for d in .dirs.d .setup .LocalDirs.d .localdirs Documents
    do
        cp -a ~/$d ~/.backup
        rm -f ~/$d
        cp -a ~/.repos/git/main/resource/userorg/main/readwrite/public/user/rc/_home/$d ~/
    done
fi


if mkdir -p ~/.backup/localdirs/home.d
then
    cd ~/.repos/git/user/localdirs/home.d
    for d in *
    do
        cp -a $d ~/.repos/git/main/resource/userorg/main/readwrite/public/user/localdirs/home.d/
    done
fi

cp -ar ~/.repos/git/main/resource/userorg/main/readwrite/public/user/localdirs/deps.d/model.d/machine.d/lispm  ~/.repos/git/main/resource/userorg/main/readwrite/public/user/localdirs/deps.d/model.d/machine.d/$HOST
ln -s $HOST ~/.repos/git/main/resource/userorg/main/readwrite/public/user/localdirs/deps.d/model.d/machine.d/default


cp -ra ~/Documents/CreatedContent/*  ~/.repos/git/main/resource/info/doc/orgs/ftnt/doc/

ll ~/.repos/git/main/resource/userorg/main/readwrite/public/user/doc/CreatedContent/contents/virtual/misc/default
ll ~/.repos/git/main/resource/userorg/main/readwrite/public/user/doc/CreatedContent/contents/virtual/muse/default
ll ~/.repos/git/main/resource/userorg/main/readwrite/public/user/doc/CreatedContent/contents/virtual/org/default
