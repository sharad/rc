set ic
set aw
set sw=4
"" set mouse=a
set title
set dir=/tmp
set nobackup writebackup
set backupdir=/tmp,.
set showmode
autocmd BufEnter *.[Cch],*.[ch]xx,*.hh,*.cc	set fo=croq cindent comments=sr:/*,mb:*,el:*/,://

map q mgMkmh3G/modified:f:D:r !dtkJx'h`g
map [19~ :w
map [20~ wbk
map [23~ :n
map [24~ 
"map [5~ 
"map [6~ 
map OH H
map OF L
map w :w
map r :n
map s :e#

if has("terminfo")
  set t_Co=16
  set t_AB=[%?%p1%{8}%<%t%p1%{40}%+%e%p1%{92}%+%;%dm
  set t_AF=[%?%p1%{8}%<%t%p1%{30}%+%e%p1%{82}%+%;%dm
else
  set t_Co=16
  set t_Sf=[3%dm
  set t_Sb=[4%dm
endif

"autocmd BufEnter *.[Cch]  source ~/c.vim
"let mysyntaxfile = "~/syntax.vim"

if &term =~ "xterm"
:silent !echo -ne "\033]12;RoyalBlue1\007"
let &t_SI = "\033]12;green\007"
let &t_EI = "\033]12;RoyalBlue1\007"
autocmd VimLeave * :!echo -ne "\033]12;green\007"
endif


"{{{ Spell check
" https://www.linux.com/learn/tutorials/357267:using-spell-checking-in-vim
set spell spelllang=en_us
"}}}

"{{{
" Emacs Mode check
" http://stackoverflow.com/questions/4236808/syntax-highlight-a-vimrc-file-in-emacs
"}}}
