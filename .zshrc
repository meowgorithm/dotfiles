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

autoload -Uz compinit
compinit
zstyle ':completion:*' list-colors 'dxfxcxdxbxegedabagacad'

if [[ HAS_BREW -eq 1 ]]; then
    export SHELL=/usr/local/bin/zsh
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
# %s the current version control system, like git or svn
# %r the name of the root directory of the repository
# %S the current path relative to the repository root directory
# %b branch information, like master
# %m in case of Git, show information about stashes
# %u show unstaged changes in the repository
# %c how staged changes in the repository
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git*' formats '(%s:%b)'

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
    PATH=$PATH:/usr/local/share/python
    PATH=$PATH:/usr/local/bin:/usr/local/sbin
    PATH=$PATH:/usr/local/share/npm/bin
    PATH=$PATH:$HOME/.cabal/bin
    PATH=$PATH:$HOME/.rvm/bin # add RVM to PATH for scripting
fi

# Z
[[ -f `brew --prefix`/etc/profile.d/z.sh ]] && . `brew --prefix`/etc/profile.d/z.sh

# Initialize RVM
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

# vim:et:ts=4:sw=4:ai
