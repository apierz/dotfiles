" Vim syntax file
" Language: Yum config
" Filenames: *.repo, /etc/yum.conf
" Maintainer: Will Woods
" Last Change: Sep 9, 2011
" Version: 0.1

if exists("b:current_syntax")
  finish
endif

syn case match
syn sync fromstart


"----- basic data types and patterns ----------------------
syn match   yumError    contained /\S.*/
syn match   yumBadLine  /^[^=:]\+$/
syn match   yumVar      /\$\%(basearch\|releasever\|arch\|uuid\|YUM\d\)/
syn match   yumOtherVar /\$\i\+/
syn match   yumComment  /^[#;].*$/ containedin=ALL
" single items
syn match   yumItem     contained /\S\+/                                                    skipwhite nextgroup=yumError
syn keyword yumBool     contained 0 1 yes no true false True False                          skipwhite nextgroup=yumError
syn match   yumInt      contained /\d\+/                                                    skipwhite nextgroup=yumError
syn match   yumDuration contained /\%(\d\+[dhm]\?\|never\)/                                 skipwhite nextgroup=yumError
" these things could be in lists
syn match   yumURL      contained '\<\%(file\|https\=\|ftp\|media\)://[^ ,]\+'  contains=yumVar
syn match   yumFile     contained /\%(\<glob:[^ ,]\+\|\/[^ ,*?]\+\)/            contains=yumVar,yumGlobSyms
syn match   yumGlobSyms contained /[*?]/
" list items
syn region  yumFileList contained start=// end=/\n\S/me=e-2 contains=yumComma,yumFile,yumError
syn region  yumURLList  contained start=// end=/\n\S/me=e-2 contains=yumComma,yumUrl,yumError
syn region  yumList     contained start=// end=/\n\S/me=e-2 contains=yumComma,yumVar
syn match   yumComma    contained /,/
"----------------------------------------------------------


"---- stuff common to main & repo config sections ---------
"TODO: this section isn't complete
syn match   yumKey      contained /^\%(keepalive\|sslverify\|gpgcheck\)\s*=\s*/ nextgroup=yumBool
syn match   yumKey      contained /^\%(retries\|timeout\)\s*=\s*/ nextgroup=yumInt,yumError
syn match   yumKey      contained /^\%(metadata_expire\|mirrorlist_expire\)\s*=\s*/ nextgroup=yumDuration,yumError
syn match   yumKey      contained /^\%(exclude\)\s*=\s*/ nextgroup=yumList
"----------------------------------------------------------


"---- [reponame] section items ----------------------------
syn region  repoRegion  matchgroup=yumHeader start=/^\[\S\+\]/ end=/^\[/me=e-2 contains=repoKey,yumKey,yumBadLine
syn match   repoKey     contained /^name\s*=\s*/ nextgroup=yumList
syn match   repoKey     contained /^\(repositoryid\)\s*=\s*/ nextgroup=yumItem
syn match   repoKey     contained /^\%(enabled\|repo_gpgcheck\|enablegroups\|skip_if_unavailable\)\s*=\s*/ nextgroup=yumBool
syn match   repoKey     contained /^\%(mirrorlist\|gpgcakey\)\s*=\s*/ nextgroup=yumUrl,yumError
syn match   repoKey     contained /^\%(baseurl\|gpgkey\)\s*=\s*/ nextgroup=yumURLList
syn match   repoKey     contained /^cost\s*=\s*/ nextgroup=yumInt
syn match   repoKey     contained /^failovermethod\s*=\s*/ nextgroup=repoFailover
syn keyword repoFailover    contained priority roundrobin skipwhite nextgroup=yumError
"----------------------------------------------------------


"---- [main] section --------------------------------------
"TODO: this section isn't complete
syn region  mainRegion  matchgroup=yumHeader start=/^\[main\]/ end=/^\[/me=e-2 contains=mainKey,yumKey,yumBadLine
syn match   mainKey     contained /^\%(installonlypkgs\|distroverpkg\|commands\)\s*=s*/ nextgroup=yumList
syn match   mainKey     contained /^\%(keepcache\|protected_multilib\|\%(local_\|repo_\)\=gpgcheck\|skip_broken\|assumeyes\|assumeno\|alwaysprompt\|tolerant\|exactarch\|showdupesfromrepos\|obsoletes\|overwrite_groups\|groupremove_leaf_only\|enable_group_conditionals\|diskspacecheck\|history_record\|plugins\|clean_requirements_on_remove\)\s*=\s*/ nextgroup=yumBool
syn match   mainKey     contained /^\%(cachedir\|persistdir\|logfile\|installroot\)\s*=\s*/ nextgroup=yumFile,yumError
syn match   mainKey     contained /^\%(reposdir\)\s*=\s*/ nextgroup=yumFileList
syn match   mainKey     contained /^\%(debuglevel\|installonly_limit\|recent\|retries\|timeout\)\s*=\s*/ nextgroup=yumInt,yumError
"keys with special values
syn match   mainKey             contained /^multilib_policy\s*=\s*/ nextgroup=yumMultilibPolicy,yumError
syn keyword yumMultilibPolicy   contained all best
syn match   mainKey             contained /^group_package_types\s*=\s*/ nextgroup=yumGroupTypeList
syn region  yumGroupTypeList    contained start=/./ end=/\n\S/me=e-2 contains=yumGroupType,yumError
syn keyword yumGroupType        contained required optional mandatory
syn match   mainKey         contained /^color\s*=\s*/ nextgroup=yumColorMode,yumError
syn keyword yumColorMode    contained always auto never
syn match   mainKey         contained /^color_list_installed_\%(older\|newer\|reinstall\|extra\)\s*=s*/ nextgroup=yumColor
syn match   mainKey         contained /^color_list_available_\%(upgrade\|downgrade\|install\|reinstall\)\s*=s*/ nextgroup=yumColor
syn match   mainKey         contained /^color_update_\%(local\|remote\|installed\)\s*=s*/ nextgroup=yumColor
syn match   mainKey         contained /^color_search_match\s*=s*/ nextgroup=yumColor
syn match   yumColor        contained /.*$/ contains=yumColorWords,yumColorSyms,yumError
syn keyword yumColorWords   contained bold blink dim reverse underline black red green yellow blue magenta cyan white
syn match   yumColorSyms    contained /\%(,\|\<fg:\|\<bg:\)/
"----------------------------------------------------------


"---- define coloring -------------------------------------
hi def link yumComment          Comment
hi def link yumHeader           Type
hi def link yumVar              PreProc
hi def link yumKey              Statement
hi def link yumError            Error
hi def link yumBadLine          Error
hi def link yumGlobSyms         Special
hi def link yumComma            Special
hi def link yumColorSyms        Special

hi def link yumURL              Constant
hi def link yumInt              Constant
hi def link yumBool             Constant
hi def link yumDuration         Constant
hi def link yumMultilibPolicy   Constant
hi def link yumGroupType        Constant
hi def link repoFailover        Constant
hi def link yumColorMode        Constant
hi def link yumColorWords       Constant

hi def link mainKey             yumKey
hi def link repoKey             yumKey

let b:current_syntax = "yumconf"
