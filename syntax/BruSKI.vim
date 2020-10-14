" Vim syntax file
" Language: BruSKI
" Maintainer: Nicklas Botö
" Latest Revision: 27 July 2020

if exists("b:current_syntax")
    finish
endif

syn keyword bruKeywords INT CHR UNL PRT EQ
hi def link bruKeywords Function

syn keyword bruImport contained import
syn keyword bruFormat contained format
syn keyword bruMacros contained macros
syn region bruLangdef start="{!" end="!}" fold transparent contains=bruImport,bruFormat,bruMacros

hi def link bruImport Include
hi def link bruFormat Define
hi def link bruMacros Preproc
hi def link bruLangdef Special

syn keyword bruDeffuncs undefined error
hi def link bruDeffuncs Preproc

syn match bruAbs /λ\+/
hi def link bruAbs Special

syn match bruAss ':='
hi def link bruAss Keyword

syn match bruDef '::'
syn match bruDef '!!'
hi def link bruDef Keyword

syn match bruVar /\w\+/ nextgroup=bruAss
" hi def link bruVar Statement

syn match bruInt '\d\+'
hi def link bruInt Number

syn region  bruString start="\"" end="\""
hi def link bruString String

syn match bruMacroArg contained /\$\w/
hi def link bruMacroArg Special

syn region bruMacro start="{+" end="+}" fold transparent contains=bruAbs,bruAss,bruDef,bruInt,bruString, bruVar, bruKeywords, bruMacroArg

syn keyword bruCommentTodo contained TODO FIXME
syn cluster bruCommentGrp  contains=bruCommentTodo
syn region  bruComment  start="--" skip="\\$" end="$" keepend contains=@bruCommentGrp
syn region  bruComment  start="{-" end="-}" contains=@bruCommentGrp
syn region  bruAnnounce start="--!--" skip="\\$" end="$" keepend contains=@bruCommentGrp
syn region  bruEvalCmnt start="-->"   skip="\\$" end="$" keepend contains=@bruCommentGrp
syn match   bruAbsComment /λ\w\+\./

hi def link bruCommentTodo Todo
hi def link bruComment     Comment
hi def link bruEvalCmnt    Todo
hi def link bruAbsComment  Special
hi def link bruAnnounce    Todo
