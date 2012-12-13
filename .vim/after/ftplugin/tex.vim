set winaltkeys=no
let g:Tex_DefaultTargetFormat = 'pdf'
call IMAP ('ITE', '\item ' ,'tex')
call IMAP ('COM', '\com{<++>}<++>','tex')
call IMAP ('URL', '\url{<++>}<++>','tex')

" let g:Tex_MultipleCompileFormats = 'pdf,dvi'
let g:Tex_CompileRule_pdf = 'rubber -d $*'
let g:Tex_UseMakefile = 0
let g:Tex_ViewRule_pdf = 'okular $* && clear'
let g:Tex_Folding = 0
let g:Tex_GotoError = 1
let g:Tex_UseSimpleLabelSearch = 1
let g:Tex_RememberCiteSearch = 1

let g:surround_{char2nr('_')} = "_{\r}"
let g:surround_{char2nr('t')} = "\\text{\r}"
let g:surround_{char2nr('i')} = "\\emph{\r}"
let g:surround_{char2nr('b')} = "\\textbf{\r}"
let g:surround_{char2nr('v')} = "\\verbatim{\r}"
let g:surround_{char2nr('o')} = "\\overline{\r}"
syn on
let g:tex_fold_enabled=1
