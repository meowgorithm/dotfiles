" Charmtone theme for vim-airline
" Color palette
let s:cumin = '#BF976F'
let s:tang = '#FF985A'
let s:yam = '#FFB587'
let s:paprika = '#D36C64'
let s:bengal = '#FF6E63'
let s:uni = '#FF937D'
let s:sriracha = '#EB4268'
let s:coral = '#FF577D'
let s:salmon = '#FF7F90'
let s:chili = '#E23080'
let s:cherry = '#FF388B'
let s:tuna = '#FF6DAA'
let s:macaron = '#E940B0'
let s:pony = '#FF4FBF'
let s:cheeky = '#FF79D0'
let s:flamingo = '#F947E3'
let s:dolly = '#FF60FF'
let s:blush = '#FF84FF'
let s:urchin = '#C337E0'
let s:crystal = '#EB5DFF'
let s:lilac = '#F379FF'
let s:prince = '#9C35E1'
let s:violet = '#C259FF'
let s:mauve = '#D46EFF'
let s:grape = '#7134DD'
let s:plum = '#9953FF'
let s:orchid = '#AD6EFF'
let s:jelly = '#4A30D9'
let s:charple = '#6B50FF'
let s:hazy = '#8B75FF'
let s:ox = '#3331B2'
let s:sapphire = '#4949FF'
let s:guppy = '#7272FF'
let s:oceania = '#2B55B3'
let s:thunder = '#4776FF'
let s:anchovy = '#719AFC'
let s:damson = '#007AB8'
let s:malibu = '#00A4FF'
let s:sardine = '#4FBEFE'
let s:zinc = '#10B1AE'
let s:turtle = '#0ADCD9'
let s:lichen = '#5CDFEA'
let s:guac = '#12C78F'
let s:julep = '#00FFB2'
let s:bok = '#68FFD6'
let s:mustard = '#F5EF34'
let s:citron = '#E8FF27'
let s:zest = '#E8FE96'
let s:caviar = '#121117'
let s:pepper = '#201F26'
let s:charcoal = '#3A3943'
let s:iron = '#4D4C57'
let s:oyster = '#605F6B'
let s:squid = '#858392'
let s:smoke = '#BFBCC8'
let s:ash = '#DFDBDD'
let s:salt = '#F1EFEF'
let s:butter = '#FFFAF1'

" Normal mode
let s:N1 = [s:butter, s:charple, 235, 141, 'bold']
let s:N2 = [s:smoke, s:charcoal, 250, 237]
let s:N3 = [s:squid, s:pepper, 246, 235]

" Insert mode
let s:I1 = [s:pepper, s:julep, 235, 49, 'bold']
let s:I2 = [s:smoke, s:charcoal, 250, 237]
let s:I3 = [s:squid, s:pepper, 246, 235]

" Visual mode
let s:V1 = [s:pepper, s:tang, 235, 215, 'bold']
let s:V2 = [s:smoke, s:charcoal, 250, 237]
let s:V3 = [s:squid, s:pepper, 246, 235]

" Replace mode
let s:R1 = [s:pepper, s:sriracha, 235, 197, 'bold']
let s:R2 = [s:smoke, s:charcoal, 250, 237]
let s:R3 = [s:squid, s:pepper, 246, 235]

" Inactive
let s:IA = [s:iron, s:charcoal, 243, 234]

" Warnings and errors
let s:warning = [s:pepper, s:tang, 235, 215, 'bold']
let s:error = [s:pepper, s:sriracha, 235, 197, 'bold']

let g:airline#themes#charmtone#palette = {}

let g:airline#themes#charmtone#palette.normal = airline#themes#generate_color_map(s:N1, s:N2, s:N3)
let g:airline#themes#charmtone#palette.insert = airline#themes#generate_color_map(s:I1, s:I2, s:I3)
let g:airline#themes#charmtone#palette.visual = airline#themes#generate_color_map(s:V1, s:V2, s:V3)
let g:airline#themes#charmtone#palette.replace = airline#themes#generate_color_map(s:R1, s:R2, s:R3)
let g:airline#themes#charmtone#palette.inactive = airline#themes#generate_color_map(s:IA, s:IA, s:IA)

" Warnings and errors
let g:airline#themes#charmtone#palette.normal.airline_warning = s:warning
let g:airline#themes#charmtone#palette.insert.airline_warning = s:warning
let g:airline#themes#charmtone#palette.visual.airline_warning = s:warning
let g:airline#themes#charmtone#palette.replace.airline_warning = s:warning

let g:airline#themes#charmtone#palette.normal.airline_error = s:error
let g:airline#themes#charmtone#palette.insert.airline_error = s:error
let g:airline#themes#charmtone#palette.visual.airline_error = s:error
let g:airline#themes#charmtone#palette.replace.airline_error = s:error
