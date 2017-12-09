set fish_greeting ""

set fish_color_autosuggestion 'green'
set fish_color_error 'bryellow'
set fish_color_command 'white'

set fish_pager_color_prefix	'green' '--bold' '--underline'
set fish_pager_color_progress	'black' '--bold' '--background=cyan'

set fish_color_user 'black' '--background=green'
set fish_color_user_swap 'green' '--background=yellow'
set fish_color_host 'black' '--background=yellow'
set fish_color_host_swap 'yellow' '--background=blue'
set fish_color_cwd  'black' '--background=blue'
set fish_color_cwd_swap  'blue' '--background=black'

# Fish git prompt
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showuntrackedfiles 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow
set __fish_git_prompt_color_upstream_ahead cyan
set __fish_git_prompt_color_upstream_behind red

set -g fish_color_git_cleanfirst 'black' '--background=cyan'
set -g fish_color_git_clean 'black' '--background=cyan'
set -g fish_color_git_stagedfirst 'black' '--background=yellow'
set -g fish_color_git_staged 'black' '--background=yellow'
set -g fish_color_git_dirtyfirst  'black' '--background=bryellow'
set -g fish_color_git_dirty  'black' '--background=bryellow'

set -g fish_prompt_git_status_added '✚'
set -g fish_prompt_git_status_modified '*'
set -g fish_prompt_git_status_renamed '➜'
set -g fish_prompt_git_status_copied '⇒'
set -g fish_prompt_git_status_deleted '✖'
set -g fish_prompt_git_status_untracked '?'
set -g fish_prompt_git_status_unmerged '!'

set -g fish_prompt_git_status_order added modified renamed copied deleted untracked unmerged

function __terlar_git_prompt --description 'Write out the git prompt'
    # If git isn't installed, there's nothing we can do
    # Return 1 so the calling prompt can deal with it
    if not command -sq git
        return 1
    end
    set -l branch (git rev-parse --abbrev-ref HEAD ^/dev/null)
    if test -z $branch
        return
    end

    echo -n ' '

    set -l index (git status --porcelain ^/dev/null|cut -c 1-2|sort -u)

    if test -z "$index"
        set_color $fish_color_git_cleanfirst
        echo -n '▖ '
        set_color $fish_color_git_clean
        echo -n $branch'✓ '
        set_color normal
        return
    end

    set -l gs
    set -l staged

    for i in $index
        if echo $i | grep '^[AMRCD]' >/dev/null
            set staged 1
        end

        switch $i
            case 'A '
                set gs $gs added
            case 'M ' ' M'
                set gs $gs modified
            case 'R '
                set gs $gs renamed
            case 'C '
                set gs $gs copied
            case 'D ' ' D'
                set gs $gs deleted
            case '\?\?'
                set gs $gs untracked
            case 'U*' '*U' 'DD' 'AA'
                set gs $gs unmerged
        end
    end

    if set -q staged[1]
        set_color $fish_color_git_stagedfirst
    else
        set_color $fish_color_git_dirtyfirst
    end

    echo -n '▖ '

    if set -q staged[1]
        set_color $fish_color_git_staged
    else
        set_color $fish_color_git_dirty
    end

    echo -n $branch

    for i in $fish_prompt_git_status_order
        if contains $i in $gs
            set -l color_name fish_color_git_$i
            set -l status_name fish_prompt_git_status_$i

            set_color $$color_name
            echo -n $$status_name
        end
    end

    set_color normal
end

function fish_prompt --description 'Write out the prompt'
  set -l last_status $status

  # User
  set_color $fish_color_user
  echo -n (whoami)
  echo -n ' '
  set_color $fish_color_user_swap
  echo -n '▙'
  set_color normal

  set_color $fish_color_host
  echo -n ' @'
  echo -n (prompt_hostname)
  echo -n ' '
  set_color $fish_color_host_swap
  echo -n '▙'
  set_color normal


  # PWD
  set_color $fish_color_cwd
  echo -n ' '
  echo -n (prompt_pwd)
  echo -n ' '
  set_color $fish_color_cwd_swap
  echo -n '▙'
  set_color normal

  __terlar_git_prompt
  echo

  if not test $last_status -eq 0
    set_color $fish_color_error
  end

  echo -n '➤ '
  set_color normal
end
