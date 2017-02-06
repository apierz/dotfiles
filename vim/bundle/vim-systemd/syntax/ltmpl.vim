" Vim syntax file
" Language:	Lorax Template
" Filenames:    *.ltmpl, */lorax/*.tmpl
" Maintainer:   Will Woods
" Last Change:	Sep. 9, 2011
" Version:	0.1

" Quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

syntax case match

syntax include @Python syntax/python.vim
unlet b:current_syntax

" TODO: split mako/ltmpl
" see /usr/share/vim/vim73/syntax/htmlcheetah.vim for an example

syn match   makoError   contained /\S.*/
syn match   makoComment /^\s*##.*/
syn region  makoTag     matchgroup=makoSpecial  start=/<\/\=%[A-Za-z0-9_.:]\+/ end=/\/\=>/ contains=makoArgs,makoString,makoVar
syn keyword makoArgs    contained args name filter file import cached buffered cache_type
syn region  makoComment matchgroup=makoSpecial  start='<%doc>' end='</%doc>'
syn region  makoText    matchgroup=makoSpecial  start='<%text>' end='</%text>'
syn region  makoPython  matchgroup=makoSpecial  start=/<%!\=\_s/ end=/%>/ contains=@Python
syn region  makoPython  matchgroup=makoSpecial  start=/^\s*%\s*\%(if\|elif\|else\|for\|while\)/ end=/$/ contains=@Python
syn match   makoSpecial /^\s*%\s*\%(endif\|endfor\|endwhile\)\>/ skipwhite nextgroup=makoError
syn region  makoVar         start=/${/ end=/}/ keepend contains=@Python,makoFilter
syn region  makoFilter      contained matchgroup=makoSpecial start=/|/ end=/}/ contains=makoFilterNames
syn keyword makoFilterNames contained u x h trim
syn region  makoString  contained start=/"/ end=/"/ contains=makoVar
syn region  makoString  contained start=/'/ end=/'/ contains=makoVar

syn match   loraxError      contained /\S.*/
syn region  loraxQuote      contained start=/"/ end=/"/ contains=makoVar,loraxContinue
syn region  loraxQuote      contained start=/'/ end=/'/ contains=makoVar,loraxContinue
syn match   loraxContinue   contained /\\$/
syn region  loraxAlt        contained matchgroup=makoSpecial start=/{/ end=/}/ contains=makoVar,loraxAltComma
syn match   loraxAltComma   contained /,/
syn match   loraxInt        contained /\s\d\+/
syn match   loraxGlobSyms   contained /[*?]/
syn cluster loraxArgClust   contains=makoVar,loraxInt,loraxQuote,loraxAlt,loraxContinue

syn region  loraxArgs       contained start=// end=/$/ contains=@loraxArgClust
syn region  loraxArgsGlob   contained start=// end=/$/ contains=@loraxArgClust,loraxGlobSyms

" match a '-' prefix on a command. (this is kind of a weak rule)
syn match loraxCmdSyms /^\s*-/

" commands that don't take globs
syn keyword loraxKey copy move hardlink symlink nextgroup=loraxArgs
syn keyword loraxKey mkdir append chmod installkernel installinitrd treeinfo nextgroup=loraxArgs

" commands that take globs
syn keyword loraxKey install installpkg removepkg remove replace runcmd nextgroup=loraxArgsGlob

" gconfset PATH TYPE VALUE [OUTFILE]
syn keyword loraxKey gconfset nextgroup=loraxArgs
"TODO hilighting for TYPE and VALUE?

" systemctl [enable|disable|mask] UNIT [UNIT...]
syn keyword loraxKey systemctl nextgroup=loraxSystemctlCmd
syn keyword loraxKey enable disable mask nextgroup=loraxArgs

" removefrom GLOB [--allbut] GLOB...
syn keyword loraxKey removefrom nextgroup=loraxRemoveFrom
syn region  loraxRemoveFrom     contained start=// end=/$/ contains=@loraxArgClust,loraxGlobSyms,loraxRemoveFromArgs
syn match   loraxRemoveFromArgs contained /--allbut\>/

" log MESSAGE
syn keyword loraxKey log skipwhite nextgroup=loraxLog
syn match   loraxLog contained /\S\+/ contains=makoVar,loraxQuote,loraxInt skipwhite nextgroup=loraxError

" run_pkg_transaction
syn keyword loraxKey run_pkg_transaction nextgroup=loraxError



" define colors
hi def link makoComment     Comment
hi def link makoText        String
hi def link makoSpecial     Special
hi def link makoVar         PreProc
hi def link makoError       Error
hi def link makoString      String
hi def link makoFilter      Constant
hi def link makoFilterNames Statement
hi def link makoArgs        Type

hi def link loraxKey        Statement
hi def link loraxAltComma   Special
hi def link loraxContinue   Special
hi def link loraxGlobSyms   Special
hi def link loraxCmdSyms    Special
hi def link loraxQuote      String
hi def link loraxError      Error
hi def link loraxInt        Constant

hi def link loraxRemoveFromArgs Special
hi def link loraxLog            String

let b:current_syntax = "ltmpl"
