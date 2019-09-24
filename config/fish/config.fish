
# Path
set -x PATH $PATH:/Users/christian/.local/bin

# FZF
set -x FZF_DEFAULT_OPTS '--margin=0,2,0,2 --height=20 --inline-info --border'
bind \cr '__fzf_reverse_isearch'

# Direnv
direnv hook fish | source

# Kitty completion
kitty + complete setup fish | source
