# FZF
set -x FZF_DEFAULT_OPTS '--margin=0,2,0,2 --height=20 --inline-info --border'
bind \cr '__fzf_reverse_isearch'

set -x PATH $PATH:/Users/christian/.local/bin

# Direnv
direnv hook fish | source
