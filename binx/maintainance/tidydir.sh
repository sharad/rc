#!/bin/zsh
# -*- major-mode: sh; -*-

# script

# [[file:~/.setup/binx/maintainance/tidydir.org::*script][script:1]]
function main()
{
  mkdir -p ~/.tidydir
  dir=$1
  dirFile=~/.tidydir/$(echo $(readlink -m $dir) | tr -s / | tr / _)

  if [ -e $dirFile ]
  then
      command ls -1 > $dirFile
  else
      dirFile_tmp=${dirFile}_tmp
      dirFile_extra=${dirFile}_extra
      comm -31 $dirFile $dirFile_tmp > $dirFile_extra
      xargs -d '\n' -a $dirFile_extra echo file extra is
  fi
}

main
# script:1 ends here
