# -*- mode: sh -*-
# This is the `rcfiles/psne.sh' file of GNU eev.
# It was created by: (find-eev "eev-rctool" "create_psnesh")
# Author and version: Eduardo Ochs, 2005jun12. Public Domain.

# To activate the `psne' command (and to define the S variable if it
# is not already defined), source this file; it works on zsh, and it
# should also work on bash and on most sh-derivatives -- but note: I
# don't use non-zsh shell very often, I might have skipped some bugs.

# The installation scripts of eev add a few lines to your .bashrc and
# .zshrc to make bash and zsh read this file on startup.
# (find-eev "eev-rctool" "new_block_bashrc")
# (find-eev "eev-rctool" "new_block_zshrc")
#
# This is a simple implementation of `psne' for bash and zsh.
# Example: "psne http://www.foo.bar/mm" will run this:
#
#   mkdir -p $S/http/www.foo.bar/ && \
#   cd       $S/http/www.foo.bar/ && \
#   wget http://www.foo.bar/mm
#
# Note that after running that "psne" we are left at the directory
# "~/snarf/http/www.foo.bar/".

export S;: ${S:=~/snarf}

function psne-sh-sed-snarfize () { sed 's,^\([a-z]*\)://,$S/\1/,'; }
function psne-sh-urlp () { echo $1 | egrep -q '^(http|ftp)://'; }
function psne-sh-meta () {(
  URL=$1
  SURL=$(echo $1 | psne-sh-sed-snarfize)
  DIR=$(dirname $SURL)
  echo "mkdir -p $DIR/ && \\"
  echo "cd       $DIR/ && \\"
  echo "wget $URL"
);}
function psne-sh () {
  if psne-sh-urlp $1; then
    eval "$(psne-sh-meta $1)"
    echo $1 >> ~/.psne.log
  else
    echo "Not an url: $1"
  fi
}
alias psne=psne-sh

# (find-sh ". $EEVTMPDIR/psne.sh; psne-sh-meta http://www.foo.bar/mm")
# (find-node "(bashref)The Set Builtin" "`e'")
