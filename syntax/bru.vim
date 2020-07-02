" Vim syntax file
" Language: BruSKI
" Maintainer: Nicklas Botö
" Latest Revision: 2 July 2020

if exists("b:current_syntax")
    finish
endif


syn keyword bruKeywords INT CHR UNL PRT
hi def link bruKeywords Keyword



syn keyword bruPreProc import define

hi def link bruPreProc PreProc


syn keyword bruCommentTodo contained TODO FIXME
syn cluster bruCommentGrp  contains=bruCommentTodo


syn region bruComment start="--" skip="\\$" end="$" keepend contains=@bruCommentGrp
syn region bruComment start="{-" end="-}" contains=@bruCommentGrp

hi def link bruCommentTodo Comment
hi def link bruComment     Comment
