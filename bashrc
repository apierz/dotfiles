#!/bin/bash

alias emc='emacsclient -nw'
alias ll='ls -lahG'
alias mkdir='mkdir -p'
alias gitla='git log --graph --oneline --all --decorate'
alias gs='git status'
alias gb='git branch'
alias gp='git push origin master'
alias rc='rails console'
alias ks='tmux kill-session'
alias df='cd ~/Documents/dotfiles'
alias nb='cd ~/Library/Application\ Support/Übersicht/widgets/nerdbar.widget' 
alias todo='emc ~/Dropbox/notes/todo.txt'
alias notes='cd ~/Dropbox/notes'
alias clock='tty-clock -c'

source ~/.git-completion.bash
source ~/.bash_git

GIT_PS1_SHOWDIRTYSTATE=1

PS1='\[\033[30;44m\]\[\033[44;90m\]\W\[\033[34;43m\]\[\033[43;90m\]$(__git_ps1 "  %s ")\[\033[33;46m\]  \[\033[0m\]\[\033[36m\]\[\033[0m\] '

export PATH="/usr/local/bin:/usr/local/sbin:/usr/local/mysql/bin:$PATH"
eval "$(rbenv init -)"

export MPD_HOST="/Users/Andy/.mpd/socket"

extract () {
   if [ -f $1 ] ; then
       case $1 in
           *.tar.bz2)   tar xvjf $1    ;;
           *.tar.gz)    tar xvzf $1    ;;
           *.bz2)       bunzip2 $1     ;;
           *.rar)       unrar x $1       ;;
           *.gz)        gunzip $1      ;;
           *.tar)       tar xvf $1     ;;
           *.tbz2)      tar xvjf $1    ;;
           *.tgz)       tar xvzf $1    ;;
           *.zip)       unzip $1       ;;
           *.Z)         uncompress $1  ;;
           *.7z)        7z x $1        ;;
           *)           echo "don't know how to extract '$1'..." ;;
       esac
   else
       echo "'$1' is not a valid file!"
   fi
 }

