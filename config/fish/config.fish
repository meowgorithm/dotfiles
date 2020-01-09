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
set -x PATH $PATH:$HOME/.config/npm/bin
