# vim:et:ts=4:sw=4:ai

# OS and package manager checks
if [[ $(uname) = 'Linux' ]]; then
    IS_LINUX=1
fi
if [[ $(uname) = 'Darwin' ]]; then
    IS_MAC=1
fi
if [[ -x `which brew` ]]; then
    HAS_BREW=1
fi
if [[ -x `which apt-get` ]]; then
    HAS_APT=1
fi
if [[ -x `which pacman` ]]; then
    HAS_PACMAN=1
fi
if [[ -x `which yum` ]]; then
    HAS_YUM=1
fi

setopt auto_name_dirs
setopt pushd_ignore_dups
setopt prompt_subst
setopt promptpercent
setopt no_beep
setopt auto_cd
setopt multios
setopt cdablevarS
setopt transient_rprompt
setopt extended_glob
setopt correct_all
zle -N self-insert url-quote-magic
autoload -U url-quote-magic
autoload -U zmv
autoload -U colors && colors
bindkey "^[m" copy-prev-shell-word

# History management
HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt hist_ignore_dups
setopt hist_reduce_blanks
setopt share_history
setopt append_history
setopt hist_verify
setopt inc_append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_space

export EDITOR=vim
export PAGER=less
export TERM=xterm-256color # make sure terminals know we can handle 256 colors A-OK

export CLICOLOR=1
export LSCOLORS=dxfxcxdxbxegedabagacad
export ls='ls -h'

autoload -U compinit && compinit

setopt auto_list
setopt auto_menu
setopt menu_complete

# Completion caching
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path .zcache
zstyle ':completion:*:cd:*' ignore-parents parent pwd

#Completion Options
zstyle ':completion:*:match:*' original only
zstyle ':completion::prefix-1:*' completer _complete
zstyle ':completion:predict:*' completer _complete
zstyle ':completion:incremental:*' completer _complete _correct
zstyle ':completion:*' completer _complete _prefix _correct _prefix _match _approximate

# Path Expansion
zstyle ':completion:*' expand 'yes'
zstyle ':completion:*' squeeze-shlashes 'yes'
zstyle ':completion::complete:*' '\\'

zstyle ':completion:*:*:*:default' menu yes select
zstyle ':completion:*:*:default' force-list always

zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

zstyle ':completion:*' list-colors 'di=33:ln=35:so=32:ex=31:lc=\e[:rc=m:ec=:mi=37;41'

compdef pkill=kill
compdef pkill=killall
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:processes' command 'ps -au$USER'

# Group matches and Describe
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:descriptions' format $'\e[01;33m -- %d --\e[0m'
zstyle ':completion:*:messages' format $'\e[01;35m -- %d --\e[0m'
zstyle ':completion:*:warnings' format $'\e[01;31m -- No Matches Found --\e[0m'

if [[ HAS_BREW -eq 1 ]]; then
    export SHELL=/usr/local/bin/zsh

    # Git completion (requires the bash-completion package in Homebrew)
    zstyle ':completion:*:*:git:*' script /usr/local/etc/bash_completion.d/git-completion.bash

else
    export SHELL=/bin/zsh
fi

# Virtualenv Stuff
VIRTUALENVWRAPPER_SCRIPT=/usr/local/share/python/virtualenvwrapper.sh
if [[ -f $VIRTUALENVWRAPPER_SCRIPT ]]; then
    export PIP_REQUIRE_VIRTUALENV=true
    export PIP_RESPECT_VIRTUALENV=true
    if [[ HAS_BREW -eq 1 ]]; then
        export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python
    fi
    export VIRTUAL_ENV_DISABLE_PROMPT=false # No need for this as we do our ourselves elsewhere (so we can set the color)
    export PIP_LOG_FILE='~/.cache/pip-log.txt' # Don't litter pip_log.txt in cwd, please
    export PIP_DOWNLOAD_CACHE='~/.cache/pip_cache' # Use a cache
    export WORKON_HOME=~/.virtualenvs
    source $VIRTUALENVWRAPPER_SCRIPT
fi

# Verson Control Information
#
# Zsh manpage, for reference:
# http://www.gsp.com/cgi-bin/man.cgi?section=1&topic=zshcontrib
#
# Routines and formatting from Seth House, Esoteric Rubbish
# http://eseth.org/2010/git-in-zsh.html

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git*:*' get-revision true
zstyle ':vcs_info:git*:*' check-for-changes true
zstyle ':vcs_info:git*' formats "(%b) %12.12i %c%u%m"
zstyle ':vcs_info:git*' actionformats "(%b|%a) %12.12i %c%u%m"
zstyle ':vcs_info:git*+set-message:*' hooks git-st git-stash

# Show remote ref name and number of commits ahead-of or behind
function +vi-git-st() {
    local ahead behind remote
    local -a gitstatus

    # Are we on a remote-tracking branch?
    remote=${$(git rev-parse --verify ${hook_com[branch]}@{upstream} \
        --symbolic-full-name 2>/dev/null)/refs\/remotes\/}

    if [[ -n ${remote} ]] ; then
        # for git prior to 1.7
        # ahead=$(git rev-list origin/${hook_com[branch]}..HEAD | wc -l)
        ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | wc -l | sed 's# ##g')
        (( $ahead )) && gitstatus+=( "${c3}+${ahead}${c2}" )

        # for git prior to 1.7
        # behind=$(git rev-list HEAD..origin/${hook_com[branch]} | wc -l)
        behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | wc -l | sed 's# ##g')
        (( $behind )) && gitstatus+=( "${c4}-${behind}${c2}" )

        hook_com[branch]="${hook_com[branch]} [${remote}${(j:/:)gitstatus}]"
    fi
}

# Show count of stashed changes
function +vi-git-stash() {
    local -a stashes

    if [[ -s ${hook_com[base]}/.git/refs/stash ]] ; then
        stashes=$(git stash list 2>/dev/null | wc -l | sed 's# ##g')
        hook_com[misc]+=" (${stashes} stashed)"
    fi
}

function precmd() {
    vcs_info
}

function virtualenv_info() {
    [ $VIRTUAL_ENV ] && echo '('`basename $VIRTUAL_ENV`') '
}

function prompt_char {
    git branch >/dev/null 2>/dev/null && echo ' ±' && return
    hg root >/dev/null 2>/dev/null && echo ' ☿' && return
    echo ' ⚡︎'
}

PROMPT='%F{yellow}$(virtualenv_info)%F{cyan}%m:%F{red}%~ %F{cyan}%n%F{grey}%F{red}$(prompt_char)%f '
RPROMPT='%F{yellow}$vcs_info_msg_0_'

# Path stuff
if [[ $IS_MAC -eq 1 ]]; then
    PATH=/usr/local/share/python:$PATH
    PATH=/usr/local/bin:$PATH
    PATH=/usr/local/sbin:$PATH
    PATH=/usr/local/share/npm/bin:$PATH
    PATH=$HOME/.cabal/bin:$PATH
    PATH=$HOME/.rvm/bin:$PATH # add RVM to PATH for scripting
fi

# Initialize Z
[[ -f `brew --prefix`/etc/profile.d/z.sh ]] && . `brew --prefix`/etc/profile.d/z.sh

# Initialize autojump (requires compunit in Zsh!)
export AUTOJUMP_IGNORE_CASE=1
[[ -s `brew --prefix`/etc/autojump.sh ]] && . `brew --prefix`/etc/autojump.sh

# Initialize RVM
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

# OS X aliases
if [[ IS_MAC -eq 1 ]]; then
    alias eth0="ipconfig getifaddr en0 | pbcopy && ipconfig getifaddr en0"
    alias eth1="ipconfig getifaddr en1 | pbcopy && ipconfig getifaddr en1"
fi

# Miscellaneous aliases
alias delete_pyc="find . -name \"*.pyc\" -exec rm -f {} \;"
alias delete_orig="find . -name \"*.orig\" -exec rm -f {} \;"

###-begin-npm-completion-###
#
# npm command completion script
#
# Installation: npm completion >> ~/.bashrc  (or ~/.zshrc)
# Or, maybe: npm completion > /usr/local/etc/bash_completion.d/npm

COMP_WORDBREAKS=${COMP_WORDBREAKS/=/}
COMP_WORDBREAKS=${COMP_WORDBREAKS/@/}
export COMP_WORDBREAKS

if type complete &>/dev/null; then
  _npm_completion () {
    local si="$IFS"
    IFS=$'\n' COMPREPLY=($(COMP_CWORD="$COMP_CWORD" \
                           COMP_LINE="$COMP_LINE" \
                           COMP_POINT="$COMP_POINT" \
                           npm completion -- "${COMP_WORDS[@]}" \
                           2>/dev/null)) || return $?
    IFS="$si"
  }
  complete -F _npm_completion npm
elif type compdef &>/dev/null; then
  _npm_completion() {
    si=$IFS
    compadd -- $(COMP_CWORD=$((CURRENT-1)) \
                 COMP_LINE=$BUFFER \
                 COMP_POINT=0 \
                 npm completion -- "${words[@]}" \
                 2>/dev/null)
    IFS=$si
  }
  compdef _npm_completion npm
elif type compctl &>/dev/null; then
  _npm_completion () {
    local cword line point words si
    read -Ac words
    read -cn cword
    let cword-=1
    read -l line
    read -ln point
    si="$IFS"
    IFS=$'\n' reply=($(COMP_CWORD="$cword" \
                       COMP_LINE="$line" \
                       COMP_POINT="$point" \
                       npm completion -- "${words[@]}" \
                       2>/dev/null)) || return $?
    IFS="$si"
  }
  compctl -K _npm_completion npm
fi
###-end-npm-completion-###
