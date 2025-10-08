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

let ox = '#3331B2'
let sapphire = '#4949FF'
let guppy = '#7272FF'

let jelly ='#4A30D9'
let charple = '#6B50FF'
let hazy = '#8B75FF'

let julep = '#00FFB2'

exe $'hi Normal guifg={smoke} guibg=NONE gui=NONE cterm=NONE'
exe $'hi CursorLine guifg=NONE guibg={charcoal} gui=NONE cterm=NONE'
exe $'hi SignColumn guifg={smoke} guibg=NONE gui=NONE cterm=NONE'
exe $'hi LineNr guifg={iron} guibg=NONE gui=NONE cterm=NONE'
exe $'hi CursorLineNr guifg={squid} guibg=NONE gui=NONE cterm=NONE'
exe $'hi CursorColumn guifg=NONE guibg={bbq} gui=NONE cterm=NONE'
exe $'hi VertSplit guifg={bbq} guibg=NONE gui=NONE cterm=NONE'
exe $'hi ColorColumn guifg=NONE guibg={bbq} gui=NONE cterm=NONE'

exe $'hi Pmenu guifg={smoke} guibg={jelly} gui=NONE cterm=NONE'
exe $'hi PmenuSel guifg={ash} guibg={charple} gui=NONE cterm=NONE'
exe $'hi PmenuSbar guifg=NONE guibg={hazy} gui=NONE ctermfg=NONE ctermbg=NONE cterm=NONE'
exe $'hi PmenuThumb guifg=NONE guibg={julep} gui=NONE cterm=NONE'

exe $'hi GitGutterAdd guifg={julep} guibg=NONE gui=NONE cterm=NONE'
" exe $'hi GitGutterChange guifg={julep} guibg=NONE gui=NONE cterm=NONE'
" exe $'hi GitGutterDelete guifg={sapphire} guibg=NONE gui=NONE cterm=NONE'

" Set terminal background color to match colorscheme
" let &t_RB = "\e]11;" .. pepper .. "\e\\"
" let &t_RF = "\e]111\e\\"
" autocmd VimLeave * silent !printf "\e]111\e\\"

set fillchars+=vert:│
