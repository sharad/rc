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
set nospell
"}}}

"{{{
" Emacs Mode check
" http://stackoverflow.com/questions/4236808/syntax-highlight-a-vimrc-file-in-emacs
"}}}






"{{{ Vim Plug
" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

" Make sure you use single quotes

" Shorthand notation; fetches https://github.com/junegunn/vim-easy-align
Plug 'junegunn/vim-easy-align'

" Any valid git URL is allowed
Plug 'https://github.com/junegunn/vim-github-dashboard.git'

" Multiple Plug commands can be written in a single line using | separators
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

" On-demand loading
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }

" Using a non-master branch
Plug 'rdnetto/YCM-Generator', { 'branch': 'stable' }

" Using a tagged release; wildcard allowed (requires git 1.9.2 or above)
Plug 'fatih/vim-go', { 'tag': '*' }

" Plugin options
Plug 'nsf/gocode', { 'tag': 'v.20150303', 'rtp': 'vim' }

" Plugin outside ~/.vim/plugged with post-update hook
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }

" Unmanaged plugin (manually installed and updated)
Plug '~/my-prototype-plugin'

" vim-orgmode
Plug 'jceb/vim-orgmode'

" Initialize plugin system
call plug#end()
"}}} Vim Plug
