set winaltkeys=no
call IMAP ('ITE', '\item ' ,'tex')
call IMAP ('COM', '\com{<++>}<++>','tex')
call IMAP ('URL', '\url{<++>}<++>','tex')

let g:Tex_DefaultTargetFormat = 'pdf'
" let g:Tex_MultipleCompileFormats = 'dvi,pdf'
let g:Tex_CompileRule_pdf = 'pdflatex -interaction=nonstopmode $*'
let g:Tex_UseMakefile = 0
let g:Tex_ViewRule_pdf = 'evince'
let g:Tex_Folding = 0
let g:Tex_MultipleCompileFormats=''

let g:surround_{char2nr('_')} = "_{\r}"
let g:surround_{char2nr('t')} = "\\text{\r}"
let g:surround_{char2nr('i')} = "\\emph{\r}"
let g:surround_{char2nr('b')} = "\\textbf{\r}"
