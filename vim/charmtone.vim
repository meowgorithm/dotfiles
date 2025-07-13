set background=dark

hi clear
let g:colors_name = 'charmtone'

let pepper = '#201F26'
let bbq = '#2d2c35'
let charcoal = '#3A3943'
let iron = '#4D4C57'
let oyster = '#605F6B'
let squid = '#858392'
let smoke = '#BFBCC8'
let ash = '#DFDBDD'
let salt = '#F1EFEF'
let butter = '#F2E9E4'

exe $'hi Normal guifg={smoke} guibg={pepper} gui=NONE cterm=NONE'
exe $'hi CursorLine guifg=NONE guibg={charcoal} gui=NONE cterm=NONE'
exe $'hi SignColumn guifg={smoke} guibg={pepper} gui=NONE cterm=NONE'
exe $'hi LineNr guifg={iron} guibg=NONE gui=NONE cterm=NONE'
exe $'hi CursorLineNr guifg={squid} guibg=NONE gui=NONE cterm=NONE'
exe $'hi CursorColumn guifg=NONE guibg={bbq} gui=NONE cterm=NONE'
exe $'hi VertSplit guifg={bbq} guibg=NONE gui=NONE cterm=NONE'
exe $'hi ColorColumn guifg=NONE guibg={bbq} gui=NONE cterm=NONE'

set fillchars+=vert:│
