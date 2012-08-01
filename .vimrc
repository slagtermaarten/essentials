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
"SNIPMATE

"Vimscripts
Bundle 'FuzzyFinder'
Bundle 'L9'
Bundle 'LaTeX-Suite-aka-Vim-LaTeX'
Bundle 'tComment'
Bundle 'vim-scripts/Vicle'
Bundle 'c.vim'
Bundle 'Vim-R-plugin'

map j gj
map k gk

let g:tex_flavor='latex'
autocmd BufRead *.tex setlocal formatoptions=l
autocmd BufRead *.tex setlocal lbr 
autocmd BufRead *.tex setlocal smartindent 
autocmd BufRead *.Rnw setlocal formatoptions=l
autocmd BufRead *.Rnw setlocal lbr 
autocmd BufRead *.Rnw setlocal smartindent 
autocmd BufRead *.txt setlocal formatoptions=l
autocmd BufRead *.txt setlocal lbr 
autocmd BufRead *.txt setlocal smartindent 
au BufRead,BufNewFile,BufReadPost *.txt set thesaurus+=~/Dropbox/thesaurus/mthesaur.txt 

command! Math w | !command cat "`pwd`/%" | math | grep -v "In\["
au BufRead *.m so ~/.vim/after/ftplugin/m.vim

set pastetoggle=<F2>
set hidden
set tabstop=4
set expandtab
set autoindent
set softtabstop=4
set shiftwidth=4
set background=light
set incsearch
" set hlsearch
let g:netrw_keepdir=0 
map f \
nmap <silent> <c-n> : NERDTreeToggle<CR>
map <leader>n :NERDTreeFind<cr>
map <leader>fm :w<cr> :!make <cr>
map <leader>es :! evince expand("%:r") & <cr>

let vimrplugin_screenplugin = 0
"Shortcut to T-Comment for commenting
" set foldlevelstart=2
map <leader>c <c-_><c-_>
nmap <leader>cs <c-_><c-_> gUU
set nobackup
set noswapfile
set noerrorbells
set wildignore=*.swp,*.bak,*.pyc,*.class
set undolevels=1000
set history=100
set pastetoggle=<F2>
nnoremap ; :
nnoremap q; q:
nnoremap ;n :n
nnoremap fm :make
nnoremap ! :! 
set autochdir
let NERDTreeChDirMode=2
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC <CR> :syntax on <CR>
nmap <silent> <leader>bi :BundleInstall<CR>
nmap <silent> <leader>tn :tabn<CR>
nmap <silent> <leader>tp :tabp<CR>
nmap <silent> <leader>sc :tabp<CR>
" nmap <leader>ck :! clearknit && knit "%:r"<CR>
nmap <leader>kk :! knit "%:r"<CR>
nmap ;ww :w<CR>
inoremap \fn <C-R>=expand("%:t")<CR>

" This beauty remembers where you were the last time you edited the file, and returns to the same position.
au BufReadPost * if line("'\"") > 0|if line("'\"") <= line("$")|exe("norm '\"")|else|exe "norm $"|endif|endif

au BufNewFile,BufReadPost *.coffee setl foldmethod=indent
au BufNewFile,BufReadPost *.coffee setl shiftwidth=4 expandtab
au BufNewFile,BufReadPost *.html nmap <silent> <c-m> :! /opt/google/chrome/google-chrome % <Cr>

au BufNewFile,BufReadPost *.html set autoindent expandtab textwidth=80
" Haskell compiler on reading of Haskell buffer
au Bufenter,BufNewFile,BufReadPost *.hs compiler ghc

autocmd FileType python set omnifunc=pythoncomplete#Complete 
autocmd BufRead,BufNewFile *.sage,*.pyx,*.spyx set filetype=python makeprg=sage\ %

au Bufenter,BufNewFile,BufReadPost *.m map <leader>lv VicleSend

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

nmap <leader>h :call RHead()<CR>
" set visualbell t_vb=
" let g:Tex_CompileRule_pdf = 'xelatex -interaction=nonstopmode $*'
let g:Tex_DefaultTargetFormat = 'pdf'
let g:Tex_MultipleCompileFormats = 'pdf'
let g:Tex_CompileRule_pdf = 'pdflatex -interaction=nonstopmode $*'
let g:Tex_UseMakefile = 0
let g:Tex_ViewRule_pdf = 'evince'
let g:Tex_Folding = 0
au BufWinLeave * mkview
au BufWinEnter * silent loadview

" set paste

fu! Showpdf() 
    let a:current_file = expand("%:r")
    ! evince current_file &
endfunction

" let g:current_file = expand("%:r")

fu! Testfunc() 
    current_file = expand("%:r")
    echo a:current_file
endfunction

noremap <C-k> :bprev<CR> 
noremap <C-l> :bnext<CR> 
set guifont=Monospace\ 9
let g:ipy_completefunc = 'local'

let g:surround_{char2nr('_')} = "_{\r}"
let g:surround_{char2nr('t')} = "\\text{\r}"
let g:surround_{char2nr('i')} = "\\emph{\r}"
let g:surround_{char2nr('b')} = "\\textbf{\r}"


