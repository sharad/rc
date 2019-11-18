#!/usr/bin/env zsh

# tmp=osetup
# trg=~/.osetup

# if [ -d ~/.setup ] ; then
#     if [ -d .dtpl/osetup ] ; then
#         [ -e $trg ] || mkdir -p $trg
#         cp -rf ~/.setup/.dtpl/$tmp/* $trg
#     else
#         echo "~/.setup/.dtpl/osetup dir is not present." >&2
#         exit -1;
#     fi
# else
#     echo "~/.setup dir is not present." >&2
#     exit -1;
# fi


# [ -d ~/.osetup ] || mkdir ~/.osetup
# mkdir -p \
#     ~/.osetup/lib/ \
#     ~/.osetup/logs/ \
#     ~/.osetup/nosecure/ \
#     ~/.osetup/nosecure/ssh/ \
#     ~/.osetup/nosecure/ssh/keys/ \
#     ~/.osetup/var/ \
#     ~/.osetup/var/cache/ \
#     ~/.osetup/var/cache/zsh/
# touch logs/msmtp nosecure/ssh/authorized_keys


# ln -s ../.Private/secure ~/.osetup/secure

# [ -e ~/.Private ] || mkdir -p ~/.Private # check for mount

# [ -e ~/.osetup ] || mkdir -p ~/.osetup
# foreach f (${(f)"$(find  ~/.osetup -type l)"}) {
#     if [ ! -e $($readlink -f  {}) ] ; then
#         mkdir -p $($readlink -f  {})
#     fi
# }

# [ -e ~/.setup/osetup ] || mkdir -p ~/.setup/osetup
# foreach f (${(f)"$(find  ~/.setup/osetup -type l)"}) {
#     if [ ! -e $($readlink -f  {}) ] ; then
#         mkdir -p $($readlink -f  {})
#     fi
# }

# foreach prioritydir (~/.Private ~/.setup/osetup ~/.setup) {
#     [ -e $prioritydir ] || mkdir -p $prioritydir
#     foreach f (${(f)"$(find $prioritydir -type l)"}) {
#         if [ ! -e $($readlink -f  {}) ] ; then
#             echo mkdir -p $($readlink -f  {})
#         fi
#     }
# }



# # you must be in ~/.setup

# if cd ~/.osetup ; then
#     foreach i ($( find ~/.osetup/ -type l )) {
#         link=$( readlink  $i )
#         if cd $(dirname $i) ; then
#             if [[ $link == *.d ]] ; then
#                 [ -d $link ] || mkdir -p $link
#             else
#                 [ -e $link ] || touch $link
#             fi
#             cd -
#         fi
#     }
#     cd -

#     if cd ~/.setup/osetup/ ; then
#         foreach i ($( find ~/.setup/osetup/ -type l )) {
#             link=$( readlink  $i )
#             if [[ $link == */.osetup/*  ]] ; then
#                 if cd $(dirname $i) ; then
#                     if [[ $link == *.d ]] ; then
#                         [ -d $link ] || mkdir -p $link
#                     else
#                         [ -e $link ] || touch $link
#                     fi
#                     cd -
#                 fi
#             fi
#         }
#         cd -
#         if cd ~/.setup ; then
#             foreach i ($( find ~/.setup -type l )) {
#                 link=$( readlink  $i )
#                 if [[ $link == */osetup/*  ]] ; then
#                     if cd $(dirname $i) ; then
#                         if [[ $link == *.d ]] ; then
#                             [ -d $link ] || mkdir -p $link
#                         else
#                             [ -e $link ] || touch $link
#                         fi
#                         cd -
#                     fi
#                 fi
#             }
#             cd -
#         fi
#     fi
# fi


unset oldd
foreach d (~/.osetup/ ~/.setup/osetup/ ~/.setup/) {
    if cd $d ; then
        foreach i ($( find $d -type l )) {
            link=$(readlink  $i)
            [ $debug  ] && echo see sdf $d $i
            if [ ! $oldd ] || [[ $link == */$(basename $oldd)/*  ]] ; then
                [ $debug  ] && echo using sdf $d
                if cd $(dirname $i) ; then
                    if [[ $link == *.d ]] ; then
                        [ -d $link ] || mkdir -p $link
                    else
                        [ -e $link ] || {
                            echo $(dirname $link)
                            [ -e $(dirname $link) ] || mkdir -p $(dirname $link) && touch $link
                        }
                    fi
                    cd -
                fi
            fi
        }
        cd -
    fi
    oldd=$d
}
unset oldd d i

