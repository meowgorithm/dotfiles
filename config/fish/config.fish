# FZF
set -x FZF_DEFAULT_OPTS '--margin=0,2,0,2 --height=20 --inline-info --border'
bind \cr '__fzf_reverse_isearch'

set -x PATH $PATH:/Users/christian/.local/bin

set -x GOPATH $HOME/.go
mkdir -p $GOPATH/{src,bin}
set -x GOBIN $GOPATH/bin
set -x PATH $PATH:/usr/local/opt/go/libexec/bin:$GOBIN:(go env GOPATH)

# Direnv
direnv hook fish | source
