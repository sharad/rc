# -*- Mode: shell-script; indent-tabs-mode: nil; sh-shell: zsh; -*-
# $Id
# ~/.zshrc: executed by zsh(1) for interactive shells.

# {{{ Usage
# Before
# if zsh is login shall
#     and RCS/GLOBAL_RCS option set; then
#     run /etc/zprofile and  then $ZDOTDIR/.zprofile
# fi

# Current
#if zsh is interactive
#     and RCS/GLOBAL_RCS option set; then
#   run /etc/zshrc and then $ZDOTDIR/.zshrc
#fi

# After
# if zsh is login shall
#     and RCS/GLOBAL_RCS option set; then
#     run /etc/zlogin and  then $ZDOTDIR/.zlogin
# fi

# Usage
#Put all setting affect intractive shall, like aliases,
#zle stuff

#Commands are then read from $ZDOTDIR/.zshenv.  If the shell is a login
#shell, commands are read from /etc/zprofile and  then  $ZDOTDIR/.zpro-
#file.   Then,  if  the  shell  is  interactive, commands are read from
#/etc/zshrc and then $ZDOTDIR/.zshrc.  Finally, if the shell is a login
#shell, /etc/zlogin and $ZDOTDIR/.zlogin are read.

# }}}

# {{{ for running ssh-agent and adding key to it.
SCREEN4KEYCHAIN=keychain
if [ "$STY" -o "$TERM" = "screen" ] && [[ "$STY" = *.${SCREEN4KEYCHAIN} ]]
then
    # date
    if [  -r ~/.rsetup/screen/env  ]
    then
        source ~/.rsetup/screen/env
        # echo source ~/.rsetup/screen/env
        # date
    fi
    if [  -r ~/.rsetup/screen/run ]
    then
        ~/.rsetup/screen/run
        # echo ~/.rsetup/screen/run
    fi
fi
# }}}

# {{{
# for r in sh zsh rc # called in ~/.zprofile
for r in zsh rc
do
    # date
    if [  -r ~/.rsetup/$r/env  ]
    then
        # echo source ~/.rsetup/$r/env
        source ~/.rsetup/$r/env
        # date
    fi
    if [  -r ~/.rsetup/$r/run ]
    then
        # echo ~/.rsetup/$r/run
        ~/.rsetup/$r/run
    fi
done
# }}}

# {{{ # For Ruby
  # [  -r ~/.rsetup/ruby/env  ] &&
  # source ~/.rsetup/ruby/env
  # [  -r ~/.rsetup/ruby/run ] &&
  # ~/.rsetup/ruby/run
[[ -r '/usr/local/lib/rvm' ]] && source '/usr/local/lib/rvm'
# }}}


emulate zsh

# {{{
if [  -r ~/.rsetup/zsh/env  ]
then
    source ~/.rsetup/zsh/env
fi
if [  -r ~/.rsetup/zsh/run ]
then
    ~/.rsetup/zsh/run
fi
# }}}

# {{{
[  -r ~/.rsetup/eev/env  ] &&
    source ~/.rsetup/eev/env
[  -r ~/.rsetup/eev/run ] &&
    ~/.rsetup/eev/run
# }}}


# If not running interactively, don't do anything:
if [ -z "$PS1" ]
then
    return
fi

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
if [ `uname -s` != "SunOS" ]
then
    if which shopt > /dev/null 2>&1
    then
	      shopt -s checkwinsize
    fi
fi

# {{{ *zModules*
###################
# [ -r ~/.zshall/modules ] && source ~/.zshall/modules
# }}}

# {{{ *Autoload*
if [ `uname -s` != "SunOS" ]
then
    autoload -U colors && colors
    autoload -Uz compinit && compinit
    autoload -U select-word-style && select-word-style bash
    autoload -U tetris	# game
    autoload -U zed	# game
fi

# }}}

# {{{ *Setopt*
#  autoload is-at-least
#  is-at-least 3.1.6-15 && setopt NO_GLOBAL_RCS
#  is-at-least 3.1.0 && setopt HIST_REDUCE_BLANKS
#  is-at-least 2.6-17 || print "You can't use is-at-least here."
        setopt DVORAK           # Dvorak
        setopt BANG_HIST # enable '!' history expansion
        setopt APPEND_HISTORY # when exiting, append history entries
                              # to $HISTFILE, rather than replacing
                              # the old file; this is the default
        setopt EXTENDED_HISTORY # save additional info to $HISTFILE
        setopt HIST_IGNORE_SPACE # if the commandline starts with a
                                 # whitespace, don't add it to history
        setopt INC_APPEND_HISTORY # append every single command to
                                  # $HISTFILE immediately after hitting ENTER.
        setopt SHARE_HISTORY # always import new commands from $HISTFILE
                             # (see 'inc_append_history' above)
        setopt AUTO_PUSHD        # add to pushd stack
        setopt PUSHD_IGNOREDUPS  # ignore dup entries
        setopt BRACE_CCL

        setopt COMPLETE_IN_WORD # If this is set, completion always
        # takes place at the cursor position in the word.


        setopt COMPLETE_ALIASES # This is quite useful to avoid having
        # to define extra completions for all your aliases.

        setopt AUTO_REMOVE_SLASH # This option is turned on by
        # default. If you complete a directory name and a slash is added
        # --- which it usually is, both to tell you that you have
        # completed a directory and to allow you to complete files
        # inside it without adding a `/' by hand --- and the next thing
        # you type is not something which would insert or complete part
        # of a file in that directory, then the slash is removed.

        setopt AUTO_PARAM_SLASH # If AUTO_PARAM_SLASH is set, then any
        # parameter expression whose value is the name of a directory
        # will have a slash appended when completed

        setopt AUTO_PARAM_KEYS # You will find that you get the
        # complete word `${PARAM}', with the closing brace and
        # (assuming there are no other matching parameters) a space
        # afterwards. However, often after you have completed a
        # parameter in this fashion you want to type something
        # immediately after it, such as a subscript. With
        # AUTO_PARAM_KEYS, if you type something at this point which
        # seems likely to have to go after the parameter name, it will
        # immediately be put there without you having to delete the
        # intervening characters

        setopt NO_ALWAYS_TO_END # If this is set, the cursor is always
        # moved to the end of the word after it is completed

        setopt LIST_TYPES # This is like the -F option to ls; files
        # which appear in the completion listing have a trailing `/' for
        # a directory, `*' for a regular file executable by the current
        # process, `@' for a link, `|' for a named pipe, `%' for a
        # character device and `#' for a block device. This option is on
        # by default.

        setopt LIST_PACKED # With LIST_PACKED, completion lists are
        # made as compact as possible by varying the widths of the
        # columns, instead of formatting them into a completely regular
        # grid.

        setopt LIST_ROWS_FIRST # With LIST_ROWS_FIRST, the listing
        # order is changed so that adjacent items appear along rows
        # instead of down columns, rather like ls's -x option.


        setopt NO_BEEP          # set, that annoying beep goes away,

        setopt NO_LIST_BEEP #, beeping is only turned off for ambiguous completions,

        # setopt AUTO_LIST # set, when the completion is ambiguous you
        # get a list without having to type ^D,

        setopt BASH_AUTO_LIST # set, the list only happens the second
        # time you hit tab on an ambiguous completion,

        setopt LIST_AMBIGUOUS #, this is modified so that nothing is
        # listed if there is an unambiguous prefix or suffix to be
        # inserted --- this can be combined with BASH_AUTO_LIST, so that
        # where both are applicable you need to hit tab three times for
        # a listing,

        # setopt REC_EXACT # , if the string on the command line exactly
        # matches one of the possible completions, it is accepted, even
        # if there is another completion (i.e. that string with
        # something else added) that also matches,


# # Please note
# zstyle ':completion:*' menu  yes select=10
# # or
# zstyle ':completion:*' menu  yes
# # or
# setopt MENU_COMPLETE
# # all three immediately start menu-select
# # without giving chance to other completion.
# # so not selecting it.

        # setopt MENU_COMPLETE # set, one completion is always inserted
        # completely, then when you hit TAB it changes to the next, and
        # so on until you get back to where you started,

        setopt AUTO_MENU #, you only get the menu behaviour when you
        # hit TAB again on the ambiguous completion.)

        setopt EQUALS
        setopt MAGIC_EQUAL_SUBST
# }}}


# P ATH variable
# p ath=( $p ath )

# fp ath=( $fp ath ~/.zshall/functions ~/.zshall/completion )
# fp ath=( $fp ath /usr/local/etc/zsh/completion )

# zsh specific options
setopt appendhistory autocd beep extendedglob nomatch notify
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
if [ `uname -s` != "SunOS" ]
then
    zstyle :compinstall filename '/home/s/hell/.zshrc'
fi
# End of lines added by compinstall

# {{{ *Aliases*
###################
if [ -r ~/.zshall/aliases ]
then
    source ~/.zshall/aliases
fi
# }}}

# {{{ *Functions*
if [ -r ~/.zshall/rcfuns ]
then
    source ~/.zshall/rcfuns
fi
# }}}

# {{{ *Variables for Zsh*
if [ -r ~/.zshall/prompt ]
then
    source ~/.zshall/prompt # set prompt.
fi

# {{{ *Zstyle*
if [ -r ~/.zshall/zstyle ]
then
    source ~/.zshall/zstyle
fi
# }}}

# {{{ *Zstyle*
if [ -r ~/.zshall/misc ]
then
    source ~/.zshall/misc
fi
# }}}

# {{{ *Zstyle*
for fp ( ~/.zshall/envfuns ~/.zshall/monitor ~/.zshall/gtd  )
do
    if [ -r $fp ]
    then
        source $fp
    fi
done
# }}}

# Lines configured by zsh-newuser-install
CVSROOT=$LOCALCVSROOT
CVSROOT=/home/c_sharad/rpstry
CVS_RSA=ssh
        # don't put duplicate lines in the history. See bash(1) for more options
# history setting
HISTCONTROL='ignoreboth:ignorespace:erasedups'
HISTFILE=~/.histfile
#HISTFILESIZE=1000000000
HISTSIZE=50000 # 1000000000
HISTIGNORE='&:ls:[bf]g:exit'
LOCALCVSROOT=/atlantic/lib/cvs/
LOGCHECK=1
PROMPT_COMMAND='history -n'
READNULLCMD=less
REPORTTIME=10	#
SAVEHIST=1000000000
SFC2VCGCVSROOT=:ext:sh4r4d@c2vcg.cvs.sourceforge.net/cvsroot/c2vcg
# TMOUT=180
# unset TMOUT             # no exit
## Watch for logins and logouts from all accounts including mine.
watch=( notme )
## Watch every 30 seconds
# logcheck=30
## Change the watch format to something more informative
# %n = username, %M = hostname, %a = action, %l = tty, %T = time,
# %W = date
WATCHFMT="%n from %M has %a tty%l at %T %W"
#=========================================================================#
# Warnning!: My Dear Please Note: Exporting This variable wn't allow	  #
# Bash script files to change directories, eg in autoconfig               #
# CDP ATH=/home/s/                                                         #
#=========================================================================#
# export http_proxy="http://192.168.10.11:8080"
# export ftp_proxy="http://192.168.10.11:8080"

# export GREP_OPTIONS='--color=auto --with-filename --context=1 --color --line-number '

## DONE in .zshenv
# export ALTERNATE_EDITOR='echo bye'
# unset EDITOR BROWSER
# if [ $DISPLAY ]
# then
#     export EDITOR=${EDITOR:-"emacsclient -d $DISPLAY -c -f ~/.emacs.d/server/general "}
#     export VISUAL=$EDITOR
#     export BROWSER=${BROWSER:-"conkeror"}
# else
#     export EDITOR=${EDITOR:-'emacsclient -t -c -f ~/.emacs.d/server/general '}
#     export VISUAL=$EDITOR
#     export BROWSER=${BROWSER:-"w3m"}
# fi

if [ $DISPLAY ]
then
    if [ "x${REMOTEEDITORHOST}" != "x" ]
    then
        EDITOR=$HOME/bin/rgeneral-xeditor
    else
        EDITOR=$HOME/bin/general-xeditor
    fi
    export VISUAL=$EDITOR
    export BROWSER="conkeror"
else
    if [ "x${REMOTEEDITORHOST}" != "x" ]
    then
        EDITOR=$HOME/bin/rgeneral-editor
    else
        EDITOR=$HOME/bin/general-editor
    fi
    export VISUAL=$EDITOR
    export BROWSER="w3m"
fi

export GREP_COLOR=31
# export GREP_OPTIONS='--color=auto' # deprecated
export LESS='--RAW-CONTROL-CHARS' # --QUIT-AT-EOF --no-init
export LESS='-R'                           #XE
export MAILCHECK=1
export TDL_DATABASE=~/.tdldb

if [ -r ~/.LESS_TERMCAP ]
then
    source ~/.LESS_TERMCAP
fi

# take mailbox from ~/.mutt/muttrc
# [ -r ~/.mutt/muttrc ] &&
# mailp ath=( ${(f)"$(sed -n \
#     's@^[[:space:]]*mailboxes[[:space:]]\+\(.*\)[[:space:]]*$@\1?new mail in ${fg_no_bold[cyan]}${_:t}${reset_color}@p'\
#     ~/.mutt/muttrc | sed  s@\^\~@$HOME@)"} )



# If console, change some keymaps
if [ $TERM = 'linux' -a -x /usr/bin/loadkeys -a -f ~/.keymap ]
then
    loadkeys ~/.keymap 2> /dev/null
fi
# }}}


# {{{ *Environment Variables for external programs*
###################
[ -r ~/.zshall/ienv-var ] && source ~/.zshall/ienv-var
# }}}



# Stty setting
stty erase  susp 

# set font cursor
setcursor		# producing empty line.

## fix this
## fetchmail -d 1   # daemon mode, poll every 4 seconds


# {{{ xterm tweaks

# ## FAQ 3.5 How do I get the meta key to work on my xterm?
# ## http://zsh.sourceforge.net/FAQ/zshfaq03.html#l21
# [[ $TERM = "xterm" ]] && stty pass8 && bindkey -me

## FAQ 3.6 How do I automatically display the directory in my xterm title bar?
## http://zsh.sourceforge.net/FAQ/zshfaq03.html#l22
##
## I modified the xterm version because it was too plain.
chpwd() {
    if [[ -t 1 ]]
    then
        case $TERM in
            sun-cmd)
                print -Pn "\e]l%~\e\\"
                ;;
            *xterm*|rxvt|(dt|k|E)term)
                print -Pn "\e]2;% [zsh $ZSH_VERSION] %n@%m: %~\a"
                ;;
        esac
    fi
}

# }}}
# {{{ Zsh FAQ entries

## FAQ 3.18: Why does zsh kill off all my background jobs when I logout?
## http://zsh.sourceforge.net/FAQ/zshfaq03.html#l34
# setopt nohup
#
## Or start jobs with &! instead of & to disown them
## (disown = don't kill at logout)

## FAQ 3.21: Why is my history not being saved?
## http://zsh.sourceforge.net/FAQ/zshfaq03.html#l37
##
## I modified this to allow for 2,000 entries instead of 200.
# HISTSIZE=2000
# HISTFILE=~/.zsh_history
# SAVEHIST=2000

## FAQ 3.23: How do I prevent the prompt overwriting output when there is no
##    newline?
## http://zsh.sourceforge.net/FAQ/zshfaq03.html#l39
##
## According to the manual, this prevents multi-line editing because the editor
## does not know where the start of the line appears.
##
# unsetopt prompt_cr

# }}}

[ -r ~/.zshall/editing ] && source ~/.zshall/editing

[ -r ~/.zshall/terminal ] && source ~/.zshall/terminal

[ -r ~/.zshall/monitor ] && source ~/.zshall/monitor

[ -r ~/.zshall/gtd ] && source ~/.zshall/gtd

# {{{ *zModules*
###################
[ -r ~/.zshall/modules ] && source ~/.zshall/modules
# }}}


# {{{ *Tramp*
# For Tramp
###################
[ -r ~/.zshall/tramp ] && source ~/.zshall/tramp
# }}}

# {{{ load all ~/.zshall/rc.d
foreach f ( ~/.zshall/rc.d/*.zsh ) {
    source $f
}

# }}}

# {{{ For changing cursor to cyan still block.	  #
if [[ "$TERM" != "dumb" ]]
then # let emacs tramp work.
#echo \[\?17\;0\;60c		# for bash
    echo -n \\033\[\?17\;0\;60c	# for zshell
fi
# }}}


# {{{ *Display Information*
# For Tramp
# "$TERM" = "dumb" for emacs tramp
if [ ! $SUDO_USER ] && [[ "$TERM" != "dumb" ]] # Not emacs tramp, let emacs tramp work.
then
    {
        # echo Hello
        # date

        local nocheck=1

        if [ -d ~/.logs/stderr/zshall/ ] && [[ $nocheck == 1 || "$(command cat ~/.logs/stderr/zshall/* 2>&1 | head -2 )" ]]
        then
            cat ~/.logs/stderr/zshall/*
            echo Could see all messages in '~/.logs/stderr/zshall/*'
        fi

        if [ -d ~/Sink/ ]
        then
            if [[ $nocheck == 1 || $(command ls -1 ~/Sink | tr -d '\n') != localPublic ]]
            then
                print 'ls ~/Sink'
                ls --color=always -F -C ~/Sink/
                echo
            fi
        fi

        if whence -p cal > /dev/null 2>&1
        then
    	      # ncal -3 -w
    	      # ncal -3
            if [[ $nocheck == 1 || $(cal -3 2>&1 | head -2 ) ]]
            then
    	          cal -3
            fi
        fi
        # display all people around, reminders and todos.
        # See who all are present and what they are doing.

        # w -l                # what all people is doing.
        w -huf                # what all people is doing.

        echo # insert a empty line

        if whence -p rem > /dev/null 2>&1
        then
            if [[ $nocheck == 1 || $(command rem -ahq 2>&1 | head -2 ) ]]
            then
                command rem -ahq >& /dev/null
            fi
        elif whence -p remind > /dev/null 2>&1
        then
            if [ -r $HOME/Documents/CreatedContent/contents/misc/remind/Reminders/init.rem ]
            then
                if [[ $nocheck == 1 || $(command remind -ahq $HOME/Documents/CreatedContent/contents/misc/remind/Reminders/init.rem 2>&1 | head -2 ) ]]
                then
                    command remind -ahq $HOME/Documents/CreatedContent/contents/misc/remind/Reminders/init.rem # >& /dev/null
                fi
            fi
        fi
        if whence -p bugz >& /dev/null
        then
            if [[ $nocheck == 1 || $(bugz  search -a spratap 2>&1 | head -2 ) ]]
            then
                bugz  search -a spratap # -s new,open,reopen
            fi
        fi

        autoload ztodo
        if whence -f ztodo >&/dev/null
        then
            if [[ $nocheck == 1 || $(ztodo list 2>&1 | head -2 ) ]]
            then
                print ztodo:
                ztodo list
            fi
        fi
        if whence -f sticky-note >&/dev/null
        then
            if [[ $nocheck == 1 || $(sticky-note -l 2>&1 | head -2 ) ]]
            then
                print sticky-note:
                sticky-note -l
            fi
        fi
        if whence -f sched >&/dev/null
        then
            if [[ $nocheck == 1 || $(sched 2>&1 | head -2 ) ]]
            then
                print sched:
                sched
            fi
        fi
        if whence -f calendar >&/dev/null
        then
            autoload calendar
            if [[ $nocheck == 1 || $(calendar -a 2>&1 | head -2 ) ]]
            then
                print calendar:
                calendar -a
            fi
        fi
        if whence -w calender | grep command >&/dev/null
        then
            if [[ $nocheck == 1 || $(command calendar 2>&1 | head -2 ) ]]
            then
                print bsd calendar:
                command calendar
            fi
        fi

        # echo Hello
        # date

    } |
command less            \
        --RAW-CONTROL-CHARS \
        --QUIT-AT-EOF       \
        --no-init
else # for emacs tramp
    #disable adding to history
    unset HISTFILE
    #HISTFILESIZE=1000000000
    HISTSIZE=0 # 1000000000
fi
# }}}


# Workplace
workoffice

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
