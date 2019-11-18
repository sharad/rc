#!/usr/bin/env zsh

foreach f ( *(.) ) {
    dir=$(echo $f | cut -d_ -f4-6 ) ;
    mkdir -p $dir ;
    cp -f $f $dir
}

foreach d ( 2015_* ) {
    7z a -p -mhe=on  $d $d
}
