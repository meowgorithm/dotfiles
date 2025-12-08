set background=dark

hi clear
let g:colors_name = 'charmtone'

let cumin = '#BF976F'
let tang = '#FF985A'
let yam = '#FFB587'
let paprika = '#D36C64'
let bengal = '#FF6E63'
let uni = '#FF937D'
let sriracha = '#EB4268'
let coral = '#FF577D'
let salmon = '#FF7F90'
let chili = '#E23080'
let cherry = '#FF388B'
let tuna = '#FF6DAA'
let macaron = '#E940B0'
let pony = '#FF4FBF'
let cheeky = '#FF79D0'
let flamingo = '#F947E3'
let dolly = '#FF60FF'
let blush = '#FF84FF'
let urchin = '#C337E0'
let crystal = '#EB5DFF'
let lilac = '#F379FF'
let prince = '#9C35E1'
let violet = '#C259FF'
let mauve = '#D46EFF'
let grape = '#7134DD'
let plum = '#9953FF'
let orchid = '#AD6EFF'
let jelly = '#4A30D9'
let charple = '#6B50FF'
let hazy = '#8B75FF'
let ox = '#3331B2'
let sapphire = '#4949FF'
let guppy = '#7272FF'
let oceania = '#2B55B3'
let thunder = '#4776FF'
let anchovy = '#719AFC'
let damson = '#007AB8'
let malibu = '#00A4FF'
let sardine = '#4FBEFE'
let zinc = '#10B1AE'
let turtle = '#0ADCD9'
let lichen = '#5CDFEA'
let guac = '#12C78F'
let julep = '#00FFB2'
let bok = '#68FFD6'
let mustard = '#F5EF34'
let citron = '#E8FF27'
let zest = '#E8FE96'
let caviar = '#121117'
let pepper = '#201F26'
let charcoal = '#3A3943'
let iron = '#4D4C57'
let oyster = '#605F6B'
let squid = '#858392'
let smoke = '#BFBCC8'
let ash = '#DFDBDD'
let salt = '#F1EFEF'
let butter = '#FFFAF1'

" Reset.
hi ColorColumn guifg=NONE guibg=NONE gui=NONE cterm=NONE
hi CursorLine guifg=NONE guibg=NONE gui=NONE cterm=NONE
hi CursorLineNr guifg=NONE guibg=NONE gui=NONE cterm=NONE
hi Folded guifg=NONE guibg=NONE gui=NONE cterm=NONE
hi LineNr guifg=NONE guibg=NONE gui=NONE cterm=NONE
hi NonText guifg=NONE guibg=NONE gui=NONE cterm=NONE
hi Normal guifg=NONE guibg=NONE gui=NONE cterm=NONE
hi PMenu guifg=NONE guibg=NONE gui=NONE cterm=NONE
hi PMenuSBar guifg=NONE guibg=NONE gui=NONE cterm=NONE
hi PMenuSel guifg=NONE guibg=NONE gui=NONE cterm=NONE
hi PMenuThumb guifg=NONE guibg=NONE gui=NONE cterm=NONE
hi Search guifg=NONE guibg=NONE gui=NONE cterm=NONE
hi SignColumn guifg=NONE guibg=NONE gui=NONE cterm=NONE
hi TabLine guifg=NONE guibg=NONE gui=NONE cterm=NONE
hi TabLineFill guifg=NONE guibg=NONE gui=NONE cterm=NONE
hi TabLineSel guifg=NONE guibg=NONE gui=NONE cterm=NONE
hi Title guifg=NONE guibg=NONE gui=NONE cterm=NONE
hi Visual guifg=NONE guibg=NONE gui=NONE cterm=NONE
hi WinSeparator guifg=NONE guibg=NONE gui=NONE cterm=NONE

" Rules.
hi ColorColumn guifg=NONE guibg={pepper} gui=NONE cterm=NONE
exe $'hi CursorLine guifg=NONE guibg={caviar} gui=NONE cterm=NONE'
exe $'hi CursorLineNr guifg={squid} guibg=NONE gui=NONE cterm=NONE'
exe $'hi Folded guifg=#767676 guibg=#2a2a2a gui=NONE cterm=NONE'
exe $'hi LineNr guifg={iron} guibg=NONE gui=NONE cterm=NONE'
exe $'hi NonText guifg={iron} guibg=NONE gui=NONE cterm=NONE'
exe $'hi Normal guifg=#C5C8C6 guibg={pepper} gui=NONE cterm=NONE'
exe $'hi Pmenu guifg={smoke} guibg={jelly} gui=NONE cterm=NONE'
exe $'hi PmenuSel guifg={ash} guibg={charple} gui=NONE cterm=NONE'
exe $'hi PmenuSbar guifg=NONE guibg={hazy} gui=NONE ctermfg=NONE ctermbg=NONE cterm=NONE'
exe $'hi PmenuThumb guifg=NONE guibg={julep} gui=NONE cterm=NONE'
exe $'hi Search guifg={charcoal} guibg={zest} gui=NONE cterm=NONE'
exe $'hi SignColumn guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi TabLine guifg={squid} guibg={caviar} gui=NONE cterm=NONE'
exe $'hi TabLineFill guifg=NONE guibg={caviar} gui=NONE cterm=NONE'
exe $'hi TabLineSel guifg={smoke} guibg={pepper} gui=NONE cterm=NONE'
exe $'hi Title guifg={malibu} guibg=NONE gui=NONE cterm=NONE'
exe $'hi Visual guifg=NONE guibg={jelly} gui=NONE cterm=NONE'
exe $'hi WinSeparator guifg={charcoal} guibg=NONE gui=NONE cterm=NONE'

exe $'hi GitGutterAdd guifg={guac} guibg=NONE gui=NONE cterm=NONE'
exe $'hi GitGutterChange guifg={tang} guibg=NONE gui=NONE cterm=NONE'
exe $'hi GitGutterDelete guifg={sapphire} guibg=NONE gui=NONE cterm=NONE'

" Tree-sitter highlight groups
exe $'hi @variable guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @variable.builtin guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @variable.parameter guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @variable.member guifg=NONE guibg=NONE gui=NONE cterm=NONE'

exe $'hi @constant guifg={zinc} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @constant.builtin guifg={thunder} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @constant.macro guifg={bengal} guibg=NONE gui=NONE cterm=NONE'

exe $'hi @module guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @module.builtin guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @label guifg=NONE guibg=NONE gui=NONE cterm=NONE'

exe $'hi @string guifg={cumin} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @string.documentation guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @string.regexp guifg={hazy} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @string.escape guifg={thunder} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @string.special guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @string.special.symbol guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @string.special.url guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @string.special.path guifg=NONE guibg=NONE gui=NONE cterm=NONE'

exe $'hi @character guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @character.special guifg=NONE guibg=NONE gui=NONE cterm=NONE'

exe $'hi @boolean guifg={zinc} guibg=NONE gui=bold cterm=NONE'
exe $'hi @number guifg={zinc} guibg=NONE gui=bold cterm=NONE'
exe $'hi @number.float guifg={zinc} guibg=bold gui=NONE cterm=NONE'

exe $'hi @type guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @type.builtin guifg={tang} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @type.definition guifg=NONE guibg=NONE gui=NONE cterm=NONE'

exe $'hi @attribute guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @attribute.builtin guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @property guifg=NONE guibg=NONE gui=NONE cterm=NONE'

exe $'hi @function guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @function.builtin guifg={guppy} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @function.call guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @function.macro guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @function.method guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @function.method.call guifg=NONE guibg=NONE gui=NONE cterm=NONE'

exe $'hi @constructor guifg={guac} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @operator guifg={salmon} guibg=NONE gui=NONE cterm=NONE'

exe $'hi @keyword guifg={mauve} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @keyword.coroutine guifg={mauve} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @keyword.function guifg={guac} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @keyword.operator guifg={tuna} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @keyword.import guifg={sapphire} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @keyword.type guifg={mauve} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @keyword.modifier guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @keyword.repeat guifg={malibu} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @keyword.return guifg={mauve} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @keyword.debug guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @keyword.exception guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @keyword.conditional guifg={mauve} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @keyword.conditional.ternary guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @keyword.directive guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @keyword.directive.define guifg=NONE guibg=NONE gui=NONE cterm=NONE'

exe $'hi @punctuation.delimiter guifg={zest} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @punctuation.bracket guifg={zest} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @punctuation.special guifg={zest} guibg=NONE gui=NONE cterm=NONE'

" Comments
exe $'hi @comment guifg={squid} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @comment.documentation guifg={squid} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @comment.error guifg={tuna} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @comment.warning guifg={zest} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @comment.todo guifg={damson} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @comment.note guifg={damson} guibg=NONE gui=NONE cterm=NONE'

exe $'hi @markup.strong guifg=NONE guibg=NONE gui=bold cterm=NONE'
exe $'hi @markup.italic guifg=NONE guibg=NONE gui=italic cterm=NONE'
exe $'hi @markup.strikethrough guifg=NONE guibg=NONE gui=strikethrough cterm=NONE'
exe $'hi @markup.underline guifg=NONE guibg=NONE gui=underline cterm=NONE'

exe $'hi @markup.heading guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @markup.heading.1 guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @markup.heading.2 guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @markup.heading.3 guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @markup.heading.4 guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @markup.heading.5 guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @markup.heading.6 guifg=NONE guibg=NONE gui=NONE cterm=NONE'

exe $'hi @markup.quote guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @markup.math guifg=NONE guibg=NONE gui=NONE cterm=NONE'

exe $'hi @markup.link guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @markup.link.label guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @markup.link.url guifg=NONE guibg=NONE gui=NONE cterm=NONE'

exe $'hi @markup.raw guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @markup.raw.block guifg=NONE guibg=NONE gui=NONE cterm=NONE'

exe $'hi @markup.list guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @markup.list.checked guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @markup.list.unchecked guifg=NONE guibg=NONE gui=NONE cterm=NONE'

exe $'hi @diff.plus guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @diff.minus guifg=NONE guibg=NONE gui=NONE cterm=NONE'
exe $'hi @diff.delta guifg=NONE guibg=NONE gui=NONE cterm=NONE'

exe $'hi @tag guifg={malibu} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @tag.builtin guifg={thunder} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @tag.attribute guifg={damson} guibg=NONE gui=NONE cterm=NONE'
exe $'hi @tag.delimiter guifg={damson} guibg=NONE gui=NONE cterm=NONE'
