# Bash completion (via Homebrew)
[[ -f `brew --prefix`/etc/bash_completion ]] && . `brew --prefix`/etc/bash_completion

# Set the terminal into 256 color mode
export TERM=xterm-256color

# Variables for later
VIRTUALENVWRAPPER_SCRIPT=/usr/local/share/python/virtualenvwrapper.sh
LAZY_VIRTUALENVWRAPPER_SCRIPT=/usr/local/share/python/virtualenvwrapper_lazy.sh
GIT_BASH_COMPLETION_SCRIPT=/usr/local/etc/bash_completion.d/git-completion.bash
BREW_BASH_COMPLETION_SCRIPT=/usr/local/etc/bash_completion.d/brew_bash_completion.sh

# For formatting the prompt
function prompt_func() {

    # Shorted the pwd in the prompt to show only the n number of characters
    # NOTE: Bash 4+ only
    PROMPT_DIRTRIM=2

    # Ascii color escape codes
    RED="\[\e[0;31m\]"
    GREEN="\[\e[0;32m\]"
    YELLOW="\[\e[0;33m\]"
    BLUE="\[\e[0;34m\]"
    PURPLE="\[\e[0;35m\]"
    CYAN="\[\e[0;36m\]"
    LIGHT_GREEN="\[\e[1;32m\]"
    WHITE="\[\e[1;37m\]"
    LIGHT_GRAY="\[\e[0;37m\]"
    COLOR_NONE="\[\e[0m\]"

    # 256 color foreground codes follow the following format:
    # SAMPLE_256_COLOR="\[\e[38;05;255m\]"

    # Show Git branch in prompt
    if [ -f $GIT_BASH_COMPLETION_SCRIPT ]; then
        export GIT_PS1_SHOWDIRTYSTATE=true
        export GIT_PS1_SHOWUNTRACKEDFILES=true
        export GIT_PS1_SHOWSTASHSTATE=true
        git_branch=$(__git_ps1 " (%s)")
    fi

    # Show virtualenv in prompt
    if [ -f $LAZY_VIRTUALENVWRAPPER_SCRIPT ]; then
        if [ "$(showvirtualenv)" == 'showvirtualenv [env]' ]; then
            virtualenv=""
        else
            virtualenv="($(showvirtualenv)) "
        fi
    fi

    PS1="${YELLOW}${virtualenv}${CYAN}\h:${RED}\w${CYAN} \u${YELLOW}${git_branch}${RED} \$${COLOR_NONE} "

}

# Format the prompt
PROMPT_COMMAND=prompt_func

# Enable Vi mode
#set -o vi

# Miscellaneous coloring and formatting
export CLICOLOR=1
export LSCOLORS=dxfxcxdxbxegedabagacad
#export LSCOLORS=ExFxCxDxBxegedabagacad
alias ls='ls -h'

# Path
export PATH="/usr/local/bin:/usr/local/sbin:$PATH"
export PATH="/usr/local/share/npm/bin:$PATH"
export PATH="/usr/local/go/bin:$PATH"
export PATH="$HOME/.cabal/bin:$PATH"
export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
export PATH=$(brew --prefix ruby)/bin:$PATH # rubygems

# Git bash completion
if [ -f $GIT_BASH_COMPLETION_SCRIPT ]; then
    source $GIT_BASH_COMPLETION_SCRIPT
fi

# Editors
#export EDITOR="/usr/bin/see -w"
export EDITOR=vim

# History Management
export HISTCONTROL=erasedups
export HISTSIZE=500
shopt -s histappend

# Keep TAR from tarring-up resource forks
export COPYFILE_DISABLE=true

# Miscellaneous aliases
alias h='history'
alias g='grep'
alias a='ack'
alias mancat='man cat'
alias redis-server='redis-server /usr/local/etc/redis.conf'
alias mongotail='tail -f /usr/local/var/log/mongodb/mongo.log'
alias apache='sudo apachectl'
alias flushdns='sudo dscacheutil -flushcache && dscacheutil -flushcache'
alias postgres_start='pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start'
alias postgres_stop='pg_ctl -D /usr/local/var/postgres stop -s -m fast'
alias postgres='postgres -D /usr/local/var/postgres'
alias reload_profile='source ~/.bash_profile'
alias tree='tree -C'
alias install_rvm='bash < <(curl -s https://raw.github.com/wayneeseguin/rvm/master/binscripts/rvm-installer)'
alias install_homebrew='ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"'
alias lock_pip='export PIP_REQUIRE_VIRTUALENV=true'
alias unlock_pip='export PIP_REQUIRE_VIRTUALENV=false'
alias delete_pyc="find . -name '*.pyc' -delete"
alias delete_orig="find . -name '*.orig' -delete"

if [ -f /usr/local/bin/vim ]; then
    alias vim='/usr/local/bin/vim'
    alias vless='/usr/local/share/vim/vim73/macros/less.sh'
fi

# Print local network adapter IPs and copy them to the clipboard
alias en0="ipconfig getifaddr en0 | pbcopy && ipconfig getifaddr en0"
alias en1="ipconfig getifaddr en1 | pbcopy && ipconfig getifaddr en1"

# For tailing the Flash error logs
alias flashtail='tail -f ~/Library/Preferences/Macromedia/Flash\ Player/Logs/flashlog.txt'
alias flashclear='rm ~/Library/Preferences/Macromedia/Flash\ Player/Logs/flashlog.txt'

# Load RVM function
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

# Initialize autojump
export AUTOJUMP_IGNORE_CASE=1
[[ -s `brew --prefix`/etc/autojump.sh ]] && . `brew --prefix`/etc/autojump.sh

# Initialize Z
[[ -f `brew --prefix`/etc/profile.d/z.sh ]] && . `brew --prefix`/etc/profile.d/z.sh

# PIP + Virtualenv Stuff
if [ -f $VIRTUALENVWRAPPER_SCRIPT ]; then
    export PIP_REQUIRE_VIRTUALENV=true
    export PIP_RESPECT_VIRTUALENV=true
    export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python
    export PIP_LOG_FILE='~/.cache/pip-log.txt' # Don't litter pip_log.txt in cwd, please
    export PIP_DOWNLOAD_CACHE='~/.cache/pip_cache' # Use a cache
    export WORKON_HOME=~/.virtualenvs
    source $LAZY_VIRTUALENVWRAPPER_SCRIPT
fi

### Begin PIP Bash completion ###
# Install the following with:
# `pip completion --bash >> ~/.profile
_pip_completion() {
    COMPREPLY=( $( COMP_WORDS="${COMP_WORDS[*]}" \
                   COMP_CWORD=$COMP_CWORD \
                   PIP_AUTO_COMPLETE=1 $1 ) )
}
complete -o default -F _pip_completion pip
### End PIP Bash completion ###

### Begin NPM Completion ###
#
# npm command completion script
#
# Installation: npm completion >> ~/.bashrc  (or ~/.zshrc)
# Or, maybe: npm completion > /usr/local/etc/bash_completion.d/npm

COMP_WORDBREAKS=${COMP_WORDBREAKS/=/}
COMP_WORDBREAKS=${COMP_WORDBREAKS/@/}
export COMP_WORDBREAKS

if complete &>/dev/null; then
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
elif compctl &>/dev/null; then
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
### End NPM Completion ###
