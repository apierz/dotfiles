" Filename:    spec.vim
" Purpose:     Vim syntax file
" Language:    SPEC: Build/install scripts for Linux RPM packages
" Maintainer:  Will Woods <wwoods@redhat.com>
" Last Change: Tue Mar 29 11:38:02 EDT 2011

if exists("b:current_syntax")
  finish
endif

syn case match
syn sync minlines=1000 " kinda dumb but specfiles are never *that* long

" include sh for the script regions
let b:is_bash=1
syntax include @Shell syntax/sh.vim
unlet b:current_syntax
" and lua for those special places where we use the embedded lua interpreter
syntax include @Lua syntax/lua.vim
unlet b:current_syntax

" comments
syn region specComment start=/^\s*#/ end=/$/ contains=specTodo
syn keyword specTodo contained FIXME NOTE TODO NOTES XXX HACK

" general stuff
syn match specURL '\%(https\?\|ftp\)://\S\+' contains=specMacro
syn match specContinue '\\$'
syn match specVersion contained '\s\%(\d\+:\)\?\d\+\%(\.\d\+\)*\%(-\d[^- ,]*\)\?'

" preamble / RPM headers
syn match specRPMHeader '^\%(Name\|Group\|Summary\|BuildRoot\|Source\d*\|Patch\d*\|URL\):'
syn match specRPMHeader '^\%(Version\|Release\):' nextgroup=specVersion
syn match specRPMHeader '^\%(Provides\|Requires\%((\%(pre\|post\|preun\|postun\))\)\?\|Conflicts\|Obsoletes\|BuildRequires\|BuildConflicts\):' nextgroup=specPRCO
syn match specPRCOOperator '[<>!]\?='
syn match specPRCO contained '.*' contains=specPRCOOperator,specVersion,specMacro,specContinue
syn match specRPMHeader '^License:\s*' nextgroup=specRPMLicenseInfo
syn match specRPMLicenseInfo contained '.*' contains=specLicense
syn match specRPMHeader '^\%(Auto\%(Req\|Prov\)\|\%(Build\|Exclusive\)\%(Arch\|OS\)\):'

" common / builtin macro names
syn keyword specMacroNames contained arm ix86 sparc nil name version release dist optflags
syn keyword specMacroNames contained buildroot _sysconfdir _prefix _exec_prefix _bindir _libdir _libexecdir _sbindir _sharedstatedir _datarootdir _datadir _includedir _infodir _mandir _localstatedir _initddir _var _usr _tmppath _usrsrc _lib _docdir
syn keyword specMacroNames contained _topdir _builddir _rpmdir _sourcedir _specdir _srcrpmdir _buildrootdir
" simple macros
syn match specMacroSym '%' contained
syn match specMacro '%[*#]' contains=specMacroSym
syn match specMacro '%\w\+' contains=specMacroNames,specMacroSym
" more complex macros
" TODO: handle %{identifier:value} better
syn region specMacro matchgroup=specMacroSym start='%{' end='}' contains=specMacroMod,specMacroNames,specMacroBuiltin,specMacro
syn match specMacroMod contained '\%(?\|!\|!?\|:\)'
syn match specMacroBuiltin contained '\%(echo\|warn\|error\|uncompress\|expand\|[SPF]\)\s*:' contains=specMacroMod
syn region specMacro matchgroup=specMacroSym start='%(' end=')' contains=@Shell,specMacro

" macro commands
syn match specMacroCommands '%\%(patch\d*\|configure\|find_lang\|makeinstall\|setup\)'
syn match specMacroCommands '%{\%(patch\d*\|configure\|find_lang\|makeinstall\|setup\)}'

" define/control
syn match specDefine '%\%(define\|undefine\|global\)' nextgroup=specIdentifier skipwhite
syn match specIdentifier '[A-Za-z0-9_]\+' contained nextgroup=specDefinition skipwhite
syn match specDefinition '.*$' contained contains=specMacro,specVersion,specContinue
syn match specControl '^%\%(ifnarch\|ifarch\|if\)' skipwhite nextgroup=specCondition
syn match specCondition '.*$' contained contains=specMacro,specVersion,specContinue,specPRCOOperator,specMacroMod
syn match specControl '^%\%(else\|endif\)\>'

" section markers
" build sections
syn match specSectionMarker '^%\%(prep\|build\|install\|check\|clean\)\>'
" metadata sections
syn match specSectionMarker '^%\%(package\|changelog\|description\|files\|policies\)\>'
" scriptlet sections
syn match specSectionMarker '^%\%(\%(pre\|post\)\%(un\|trans\)\?\|verifyscript\|trigger\%(in\|un\|prein\|postun\)\?\)'

" scriptlet + build sections are shell..
syn region specSectionShell matchgroup=specSectionMarker start='^%\%(prep\|build\|install\|check\|clean\|\%(pre\|post\)\%(un\|trans\)\?\|verifyscript\|trigger\%(in\|un\|prein\|postun\)\?\)' end='^%\%(prep\|build\|install\|check\|cleanpackage\|changelog\|description\|files\|policies\|\%(pre\|post\)\%(un\|trans\)\?\|verifyscript\|trigger\%(in\|un\|prein\|postun\)\?\)\>'me=s-1 contains=specMacro,specMacroNames,specControl,specDefine,specMacroCommands,@Shell
" unless we request a different interpreter..
syn region specSectionShell matchgroup=specSectionMarker start='^%\%(prep\|build\|install\|check\|clean\|\%(pre\|post\)\%(un\|trans\)\?\|verifyscript\|trigger\%(in\|un\|prein\|postun\)\?\).*\s-p\s*<lua>' end='^%\%(prep\|build\|install\|check\|cleanpackage\|changelog\|description\|files\|policies\|\%(pre\|post\)\%(un\|trans\)\?\|verifyscript\|trigger\%(in\|un\|prein\|postun\)\?\)\>'me=s-1 contains=specMacro,specMacroNames,specControl,specDefine,specMacroCommands,@Lua
" changelog section
syn region specChangeLog matchgroup=specSectionMarker start='^%changelog\>' end='^%' contains=specChangelogHeader,specURL,specBugID


" This covers the most common licenses in Fedora
syn keyword specLicense contained AFL BSD CC0 CDDL CeCILL FTL GFDL ISC MIT NCSA OFL PHP SISSL TCL TORQUEv1.1 UCD W3C ZPLv2.1
syn keyword specLicense contained Boost ImageMagick Lucida Netscape OpenPBS OpenSSL PostgreSQL Python Ruby Utopia wxWidgets zlib
syn match specLicense contained '\<\([CEMQST]\|ER\|LP\|NG\|WTF\)PL\>'
syn match specLicense contained '\<[AL]\?GPL\(v[23]\)\?+\?\( with exceptions\?\| or Artistic\)\?'
syn match specLicense contained '\<\(ASL \(1.[01]\|2.0\)\>\|Artistic \(clarified\|2.0\)\|MPLv1.[01]\)\>'
syn match specLicense contained '\<CC-BY\(-SA\|-ND\)\?\>'
syn match specLicense contained '\<\(MIT\|BSD\) with advertising\>'
syn match specLicense contained '\<\(Public Domain\|Copyright only\|Freely redistributable without restriction\|Redistributable, no modification permitted\)\>'

" Changelog stuff
syn keyword specWeekday contained Mon Tue Wed Thu Fri Sat Sun
syn keyword specWeekday contained Monday Tuesday Wednesday Thursday Friday Saturday Sunday
syn keyword specMonth   contained Jan Feb Mar Apr Jun Jul Aug Sep Oct Nov Dec
syn keyword specMonth   contained January February March April May June July August September October November December
syn match specDate      contained '\u\l\+ \+\u\l\+ \+\d\d\? \+\d\d\d\d' contains=specWeekday,specMonth
syn match specEmail     contained '<.\+>'
syn match specEmail     contained "<\?\S\+@\%([A-Za-z0-9_-]\+\.\)\+\a\+>\?"
syn match specChangelogHeader contained '^\*.*$' contains=specDate,specEmail,specVersion

syn match specBugID contained '\%([Bb]ug\|\a*[Bb][Zz]\)[ #]*\d\+'

"####################################

" Define the default highlighting.
hi def link specComment             Comment
hi def link specTodo                Todo
hi def link specRPMHeader           Statement
hi def link specDefine              PreProc
hi def link specControl             Identifier
hi def link specMacroCommands       Macro
hi def link specIdentifier          Identifier
hi def link specMacroSym            Special
hi def link specMacroMod            Operator
hi def link specSectionMarker       Structure
hi def link specURL                 PreProc
hi def link specEmail               PreProc
hi def link specVersion             Constant
hi def link specBugID               Constant
hi def link specPRCOOperator        Operator
hi def link specContinue            Operator
hi def link specDate                String
hi def link specLicense             String

" yes, it's ugly, but white is sooo cool
if &background == "dark"
  hi def specMacroNames             ctermfg=white
else
  hi def link specMacroNames        Identifier
endif

  "colors mapped onto other things
hi def link specRPMHeaderSimple     specRPMHeader
hi def link specRPMHeaderVersion    specRPMHeader
hi def link specMacroBuiltin        specMacroNames
hi def link specWeekday             specDate
hi def link specMonth               specDate

let b:current_syntax = "spec"
