set nocompatible
filetype off
syntax enable
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
filetype plugin on
filetype indent on

Bundle 'gmarik/vundle'
Bundle 'ervandew/supertab'
Bundle 'scrooloose/nerdtree'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'
Bundle 'rosenfeld/conque-term'
Bundle 'bundacia/ScreenPipe'
Bundle 'panozzaj/vim-autocorrect'
Bundle 'craigemery/vim-autotag'
Bundle "ivanov/vim-ipython"
"SNIPMATE
Bundle "MarcWeber/vim-addon-mw-utils"
Bundle "tomtom/tlib_vim"
Bundle "snipmate-snippets"
Bundle "garbas/vim-snipmate"
Bundle "Raimondi/delimitMate"
Bundle "majutsushi/tagbar"
Bundle "altercation/vim-colors-solarized"
"SNIPMATE

"Vimscripts
Bundle 'L9'
Bundle 'FuzzyFinder'
Bundle 'fugitive.vim'
Bundle 'LaTeX-Suite-aka-Vim-LaTeX'
Bundle 'tComment'
Bundle 'vim-scripts/Vicle'
Bundle 'c.vim'
Bundle 'Vim-R-plugin'

if has('gui_running')
    au FocusLost * silent! wa
endif

map j gj
map k gk
set pastetoggle=<F9>>
imap ii <Esc>

autocmd BufRead *.tex,*.Rnw,*.txt setlocal formatoptions=l
autocmd BufRead *.tex,*.Rnw,*.txt setlocal lbr 
autocmd BufRead *.tex,*.Rnw,*.txt setlocal smartindent 
au BufRead,BufNewFile,BufReadPost *.txt,*.tex set thesaurus+=~/Dropbox/thesaurus/mthesaur.txt 

command! Math w | !command cat "`pwd`/%" | math | grep -v "In\["
au BufRead *.m so ~/.vim/after/ftplugin/mathematica.vim

" nnoremap <F2> :set invpaste paste?<CR>
set pastetoggle=<F2>
set showmode
set hidden
set tabstop=4
set expandtab
set autoindent
set smartindent
set softtabstop=4
set shiftwidth=4
set background=light
set incsearch
" set hlsearch
let g:netrw_keepdir=0 
map f \
nmap <silent> <c-n> : NERDTreeToggle<CR>
map <leader>n :NERDTreeFind<cr>
" nmap <silent> <leader>w : wa <cr>
map <silent> <leader>w :wa <cr>:! make all<cr>

let vimrplugin_screenplugin = 0
"Shortcut to T-Comment for commenting
set foldlevelstart=2
map <leader>c <c-_><c-_>
nmap <leader>cs <c-_><c-_> gUU
set nobackup
set noswapfile
set noerrorbells
set wildignore=*.swp,*.bak,*.pyc,*.class
set undolevels=1000
set history=100
nnoremap ; :
nnoremap q; q:
nnoremap ;n :n
nnoremap fm :make
nnoremap ! :! 
" set autochdir
nnoremap ,cd :cd %:p:h<CR>
let NERDTreeChDirMode=2
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>em :e Makefile<CR>
nmap <silent> <leader>sv :so $MYVIMRC <CR> :syntax on <CR>
nmap <silent> <leader>bi :BundleInstall<CR>
nmap <silent> <leader>tn :tabn<CR>
nmap <silent> <leader>tp :tabp<CR>
nmap <silent> <leader>sc :tabp<CR>
" nmap <leader>ck :! clearknit && knit "%:r"<CR>
nmap <leader>kk :! knit "%:r"<CR>
nmap ;ww :w<CR>
inoremap \fn <C-R>=expand("%:t")<CR>

" " This beauty remembers where you were the last time you edited the file, and returns to the same position.
" au BufReadPost * if line("'\"") > 0|if line("'\"") <= line("$")|exe("norm '\"")|else|exe "norm $"|endif|endif

au BufNewFile,BufReadPost *.coffee setl foldmethod=indent
au BufNewFile,BufReadPost *.coffee setl shiftwidth=4 expandtab
au BufNewFile,BufReadPost *.html nmap <silent> <c-m> :! /opt/google/chrome/google-chrome % <Cr>
" au BufNewFile,BufReadPost *.tex set syntax=tex

au BufNewFile,BufReadPost *.html set autoindent expandtab textwidth=80
" Haskell compiler on reading of Haskell buffer
au Bufenter,BufNewFile,BufReadPost *.hs compiler ghc

autocmd FileType python set omnifunc=pythoncomplete#Complete 
" autocmd BufRead,BufNewFile *.sage,*.pyx,*.spyx set filetype=python makeprg=sage\ %

if v:progname =~? "evim"
  finish
endif

set backspace=indent,eol,start

if has("vms")
  set nobackup		" do not keep a backup file, use versions instead
else
endif

if has('mouse')
  set mouse=a
endif

if has("autocmd")
  augroup vimrcEx
  au!
  autocmd FileType text setlocal textwidth=80
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  augroup END
endif " has("autocmd")

if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
		  \ | wincmd p | diffthis
set autochdir
endif

autocmd FileType python let w:vicle_selection_string = "0v}y"
autocmd FileType R let w:vicle_selection_string = "0v}y"
" let g:tex_indent_items = 1

noremap <C-k> :bprev<CR> 
noremap <C-l> :bnext<CR> 
" set guifont=Monospace\ 8.5
" colorscheme solarized
set background=light
" let g:ipy_completefunc = 'local'
let g:tex_flavor='latex'
" au BufRead *.tex *.Rnw so ~/.vim/after/ftplugin/tex.vim

fu! DoRunPyBuffer2() 
    pclose! " force preview window closed"
    setlocal ft=python

    " copy the buffer into a new window, then run that buffer through python
    sil %y a | below new | sil put a | sil %!python -
    " " indicate the output window as the current previewwindow
    " setlocal previewwindow ro nomodifiable nomodified
    setlocal previewwindow
    "
    " " back into the original window
    winc p
endfu
command! RunPyBuffer call DoRunPyBuffer2() 
map <Leader>p :RunPyBuffer<CR>

set previewheight=15
au BufEnter ?* call PreviewHeightWorkAround()
func! PreviewHeightWorkAround()
    if &previewwindow
        exec 'setlocal winheight='.&previewheight
    endif
endfunc

function! g:ChmodOnWrite()
  if v:cmdbang
    silent !chmod u+w %
  endif
endfunction

autocmd BufWrite * call g:ChmodOnWrite()

let g:Tex_DefaultTargetFormat = 'pdf'
let g:Tex_ViewRule_pdf = 'okular'
let g:tex_fold_enabled=1

autocmd User fugitive
  \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
  \   nnoremap <buffer> .. :edit %:h<CR> |
  \ endif

autocmd BufReadPost fugitive://* set bufhidden=delete
set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P
set laststatus=2
nmap <F8> :TagbarToggle<CR>

set sessionoptions=blank,buffers,curdir,folds,winsize,slash,unix
au BufWinLeave *.tex mkview
au BufWinEnter *.tex silent loadview
let Tlist_GainFocus_On_ToggleOpen = 1

set wildmenu
set wildignore=*.o,a.out,*.bbl,*.pdf
set so=10
