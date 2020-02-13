# Path
set -x PATH $PATH:$HOME/.local/bin

# FZF
set -x FZF_DEFAULT_OPTS '--margin=0,2,0,2 --height=20 --inline-info --border'
bind \cr '__fzf_reverse_isearch'

# Direnv
direnv hook fish | source

# Kitty completion
kitty + complete setup fish | source

# Go
set -x GOPATH $HOME/.go
mkdir -p $GOPATH/{src,bin}
set -x PATH $PATH:/$GOPATH/bin
set -x GOPRIVATE github.com/charmbracelet

# NPM
mkdir -p $HOME/.config/npm
npm config set prefix $HOME/.config/npm
set -x PATH $HOME/.config/npm/bin:$PATH

# Aliases
set -x MEOW 'git@github.com:meowgorithm'
set -x MAGIC 'git@github.com:magicnumbers'
set -x BIT 'https://meowgorithm@bitbucket.org/meowgorithm'
set -x GITLAB 'git@gitlab.com:meowgorithm'
set -x CHARM 'git@github.com:charmbracelet'
