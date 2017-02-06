" Filename:    udev.vim
" Purpose:     Vim syntax file
" Language:    udev rules files
" Maintainer:  Will Woods <wwoods@redhat.com>
" Last Change: Thu Feb 23 14:59:36 EST 2012

if exists("b:current_syntax")
  finish
endif

syntax case match
syntax sync maxlines=10

syntax region udev_comment start=/#/ end=/$/

syntax match udev_match /[A-Z_]\+\%({[^}]\+}\)\?\%(==\|!=\)"[^"]\+"/ contains=udev_match_keys,udev_brace,udev_str
syntax match udev_assign /[A-Z_]\+\%({[^}]\+}\)\?\%(=\|:=\|+=\)"[^"]\+"/ contains=udev_assign_keys,udev_label_keys,udev_brace,udev_str

syntax keyword udev_match_keys contained ACTION DEVPATH KERNEL KERNELS NAME SYMLINK SUBSYSTEM SUBSYSTEMS DRIVER DRIVERS TAG TAGS PROGRAM RESULT ENV ATTR ATTRS TEST
syntax keyword udev_assign_keys contained NAME SYMLINK OWNER GROUP MODE TAG RUN WAIT_FOR OPTIONS ENV ATTR IMPORT

syntax region udev_brace contained start=/{/hs=s+1 end=/}/he=e-1 contains=udev_attr_special
syntax match udev_attr_special contained '\[\i\+/\i\+\]'

syntax keyword udev_label_keys contained LABEL GOTO

" strings and string subs
syntax region udev_str contained start=/"/ end=/"/ contains=udev_globs,udev_subs,udev_bad_subs
syntax match  udev_globs contained /[?|*]/
syntax region udev_globs contained start=/\[/ end=/\]/
syntax match udev_escapes contained /\$\$\|%%/
syntax match udev_subs contained /%[knpbMmcsE]/
syntax match udev_subs contained /\$\%(kernel\|number\|devpath\|id\|driver\|major\|minor\|result\|parent\|name\|links\|root\|sys\|tempnode\)/
syntax region udev_subs contained start=/$\%(attr\|env\){/ end=/}/


hi def link udev_comment        Comment
hi def link udev_match_keys     Identifier
hi def link udev_assign_keys    Operator
hi def link udev_label_keys     Structure
hi def link udev_brace          Macro
hi def link udev_str            String
hi def link udev_subs           Macro
hi def link udev_globs          Special
hi def link udev_escapes        Special
hi def link udev_attr_special   Special
