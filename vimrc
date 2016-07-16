"      Andy's .vimrc file
"      andy@andypierz.com
"      |nm|
"          |-twitter|
"          |--website---|


syntax on
set number
set ai
set wm=8
set laststatus=2
set autoread
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'

call vundle#end()            " required
filetype plugin indent on    " required

