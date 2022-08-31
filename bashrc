#!/usr/bin/env bash

# Where our dotfiles repo lives
export DOTFILES="$HOME/.dotfiles"

[ -f "$DOTFILES/bash_funcs" ] && . "$DOTFILES/bash_funcs"

# If not running interactively, don't do anything else
[[ $- != *i* ]] && return

os="$(which_os)"

function thunderboltUp() {
    local "$id"
    case "$HOSTNAME" in
    stardust)
        id="00968984-4c6a-1e00-ffff-ffffffffffff"
        ;;
    *) # lightningbolt
        id="00b62260-4590-1e00-ffff-ffffffffffff"
        ;;
    esac
    sudo boltctl authorize "$id"
}

# OS/Distro specific stuff
case "$os" in

    void )

        alias ls='ls --color=auto'
        PS1='[\u@\h \W]\$ '

        # Git prompt
        git_prompt="/usr/share/git/git-prompt.sh"
        if [ -f $git_prompt ]; then
            export GIT_PS1_SHOWDIRTYSTATE=true
            export GIT_PS1_SHOWUNTRACKEDFILES=true
            export GIT_PS1_SHOWSTASHSTATE=true
            # shellcheck disable=1090
            source $git_prompt
        fi

        # Services
        alias sv="sudo sv"

        # General aliases
        alias hey="nohup chromium --new-window --app=https://app.hey.com > /dev/null 2>&1 &"
        alias calendar="nohup chromium --new-window --app=https://calendar.google.com/calendar/u/2 > /dev/null 2>&1 &"
        alias thunderbolt-up=thunderboltUp

        # Fixes for various things on demand
        alias make-firefox-default="xdg-settings set default-web-browser firefox.desktop"
        alias fix-display='xrandr --output DP-0 --mode 2560x2880 --pos 0x0 --output DP-2 --mode 2560x2880 --pos 2560x0'

        # CUDA
        export PATH="$PATH:/usr/local/cuda-11.2/bin"
        export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/local/cuda-11.2/lib64"

        # VSCode
        vsCodePath=/usr/local/vscode/bin
        [[ -d $vsCodePath ]] && PATH="$PATH:$vsCodePath"

        # Blender
        blenderPath=/usr/local/blender
        [[ -d $blenderPath ]] && PATH="$PATH:$blenderPath"

        ;;

    debian )

        # shellcheck disable=1091
        if ! shopt -oq posix; then
            if [ -f /usr/share/bash-completion/bash_completion ]; then
                . /usr/share/bash-completion/bash_completion
            elif [ -f /etc/bash_completion ]; then
                . /etc/bash_completion
            fi
        fi
        ;;

    darwin )

        # Brew on Apple Silicon lives in /opt/homebrew and needs to initialize
        # in the shell with a script.
        if [[ $(uname -m) == "arm64" ]]; then
            eval "$(/opt/homebrew/bin/brew shellenv)"
        fi

        # Make sure XDG_CONFIG_HOME is a thing
        if [[ -z $XDG_CONFIG_HOME ]]; then
            export XDG_CONFIG_HOME="$HOME/.config"
        fi
        mkdir -p "$XDG_CONFIG_HOME"

        export CLICOLOR=1
        export LSCOLORS=dxfxcxdxbxegedabagacad
        alias ls='ls -h'

        git_completion="$(brew --prefix)/etc/bash_completion.d/git-completion.bash"
        if [ -f "$git_completion" ]; then
            export GIT_PS1_SHOWDIRTYSTATE=true
            export GIT_PS1_SHOWUNTRACKEDFILES=true
            export GIT_PS1_SHOWSTASHSTATE=true
        fi

        # Bash completion
        bash_completion="$(brew --prefix)/etc/profile.d/bash_completion.sh"
        # shellcheck disable=1090
        [[ -r "$bash_completion" ]] && . "$bash_completion"

        # Keep TAR from tarring-up resource forks
        export COPYFILE_DISABLE=true

        # History Management
        shopt -s histappend
        export HISTCONTROL=ignoredups:erasedups
        export HISTSIZE=1000

        # Miscellaneous aliases
        alias flushdns='sudo dscacheutil -flushcache && dscacheutil -flushcache'
        alias install_kitty='curl -L https://sw.kovidgoyal.net/kitty/installer.sh | sh /dev/stdin'
        alias fix_spotlight="find . -type d -path './.*' -prune -o -path './Pictures*' -prune -o -path './Library*' -prune -o -path '*node_modules/*' -prune -o -type d -name 'node_modules' -exec touch '{}/.metadata_never_index' \; -print"

        # Print local network adapter IPs and copy them to the clipboard
        alias en0="ipconfig getifaddr en0 | pbcopy && ipconfig getifaddr en0"
        alias en1="ipconfig getifaddr en1 | pbcopy && ipconfig getifaddr en1"

        # Colored man pages
        export LESS_TERMCAP_mb=$'\E[01;31m'
        export LESS_TERMCAP_md=$'\E[01;31m'
        export LESS_TERMCAP_me=$'\E[0m'
        export LESS_TERMCAP_se=$'\E[0m'
        export LESS_TERMCAP_so=$'\E[01;44;33m'
        export LESS_TERMCAP_ue=$'\E[0m'
        export LESS_TERMCAP_us=$'\E[01;32m'

        # Node
        export PATH="$PATH:$HOME/.config/npm/bin"

        node16_amd64="/usr/local/opt/node@16/bin"
        if [ -d "$node16_amd64" ]; then
            export PATH="$PATH:$node16_amd64"
        fi

        node16_arm64="/opt/homebrew/opt/node@16/bin"
        if [ -d "$node16_arm64" ]; then
            export PATH="$PATH:$node16_arm64"
        fi

        ;;

esac

# Shorten the pwd in the prompt to show only the n number of characters
# NOTE: Bash 4+ only
PROMPT_DIRTRIM=2

function nixPrompt() {
    case $IN_NIX_SHELL in
        pure)   printf ' <nix>'  ;;
        impure) printf ' <nix*>' ;;
        *)      printf ''        ;;
    esac
}

red='\[\e[0;31m\]'
yellow='\[\e[0;33m\]'
cyan='\[\e[0;36m\]'
violet='\[\e[38;5;63m\]'
no_color='\[\e[0m\]'
indigo='\[\e[38;2;90;86;224m\]'

function prompt_func() {
    nix=$(nixPrompt)

    git_branch=$(__git_ps1 " (%s)")

    if [[ -n $DEMO_PROMPT ]]; then
        PS1="$indigo>$no_color "
    else
        PS1="$cyan\h:$red\w $cyan\u$yellow$git_branch$violet$nix $red\$ $no_color"
    fi
}

PROMPT_COMMAND=prompt_func

gpg_TTY=$(tty)
export GPG_TTY=$gpg_TTY
export EDITOR=kak
export HISTCONTROL=ignoredups:erasedups
export PATH="$PATH:$HOME/.bin"

if [[ "$OSTYPE" == "linux" || "$OSTYPE" == "linux-gnu" ]]; then
    export PATH="$PATH:$HOME/.bin-linux"
fi

alias delete_pyc='find . -name '\*.pyc' -delete'
alias delete_orig='find . -name '\*.orig' -delete'
alias lock_pip='export PIP_REQUIRE_VIRTUALENV=true'
alias unlock_pip='export PIP_REQUIRE_VIRTUALENV=false'
alias ggbt='go get github.com/charmbracelet/bubbletea@latest'
alias ggb='go get github.com/charmbracelet/bubbles@latest'
alias gglg='go get github.com/charmbracelet/lipgloss@latest'
alias lw='ssh localhost -p 23231'

if command -v exa &> /dev/null; then
    alias tree='exa --tree'
elif command -v tree &> /dev/null; then
        alias tree='tree -C'
fi

# Kitty
if [[ $TERM = 'xterm-kitty' ]]; then
    # This has proved to work better than the ssh kitten for some reason.
    alias ssh='TERM=xterm-256color && ssh'
fi
# shellcheck disable=1091
if test -n "$KITTY_INSTALLATION_DIR" -a -e "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash";
    then source "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash";
fi

# Go
export GOPATH="$HOME/.go"
export GOBIN="$GOPATH/bin"
export PATH="$PATH:$GOBIN"

# GHCUP
# shellcheck disable=1091
[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"

# Cargo
export PATH="$PATH:$HOME/.cargo/bin"

# Nix
export PATH="$PATH:$HOME/.nix-profile/bin"

# Helper vars
export MEOW='git@github.com:meowgorithm'
export MAGIC='git@github.com:magicnumbers'
export BIT='https://meowgorithm@bitbucket.org/meowgorithm'
export GITLAB='git@gitlab.com:meowgorithm'
export CHARM='git@github.com:charmbracelet'
export GOCHARM='github.com/charmbracelet'
export GOMEOW='github.com/charmbracelet'

gmr() {
    if [[ $# != 2 ]]; then
        printf 'We need two arguments\n'
        return
    fi
    go mod edit -replace "github.com/charmbracelet/$1=$2"
}

# Git
export GOPRIVATE="github.com/charmbracelet"

# Direnv
command -v direnv > /dev/null 2>&1 && eval "$(direnv hook bash)"

# Z
# shellcheck disable=1091
if [[ "$os" == "void" || "$os" == "debian" ]]; then
    [[ -r "/usr/local/bin/z.sh" ]] && source /usr/local/bin/z.sh
elif [[ "$os" == "darwin" ]];  then
    [[ -r $(brew --prefix)/etc/profile.d/z.sh ]] && . "$(brew --prefix)/etc/profile.d/z.sh"
fi

# z.lua
zLuaPath=""
case "$os" in
    void )
        zLuaPath="/usr/local/bin/z.lua" ;;
    darwin )
        zLuaPath="$(brew --prefix)/opt/z.lua/share/z.lua/z.lua" ;;
esac
if [[ -r "$zLuaPath" ]]; then
    eval "$(lua "$zLuaPath" --init bash enhanced once echo)"
fi

# shellcheck disable=1090
command -v gpg > /dev/null 2>&1 && \
    source <(gpg --decrypt "$DOTFILES/rc.gpg" 2> /dev/null)

command -v exa > /dev/null 2>&1 && \
    alias ls='exa -g'
