" Clear Defaults {{{
hi Comment guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Constant guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi String guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Character guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Number guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Boolean guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Float guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Identifier guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Function guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Statement guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Conditional guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Repeat guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Label guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Operator guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Keyword guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Exception guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi PreProc guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Include guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Define guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Macro guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi PreCondit guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Type guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi StorageClass guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Structure guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Typedef guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Special guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi SpecialChar guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Tag guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Delimiter guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi SpecialComment guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
hi Debug guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
" }}}

" User Interface {{{

hi ColorColumn guibg=#202020
hi Constant gui=bold
hi CursorLine guibg=#090909 gui=NONE cterm=NONE
hi LineNr guifg=#4d4d4d
hi CursorLineNr guifg=#8c8c8c gui=NONE cterm=NONE
hi Normal guifg=#C5C8C6 gui=NONE cterm=NONE
hi Search guifg=#202020 guibg=#dcf764
hi Visual guibg=#4128bd

hi NonText guifg=#414141
hi PMenu guifg=#a8a8a8 guibg=#090909
hi PMenuSel guifg=#c2a3e0 guibg=#5f00ff
hi PMenuSBar guibg=#005f5f
hi PMenuThumb guibg=#00af87
hi SignColumn guibg=NONE
hi VertSplit guifg=#333333 guibg=NONE cterm=NONE

" }}}

" General {{{

" hi Comment guifg=#00875f
hi Comment guifg=#676767

hi Constant guifg=#05ffb0
hi String guifg=#C69669
hi Character guifg=#05ffb0
hi Number guifg=#05ffb0
hi Boolean guifg=#05ffb0
hi Float guifg=#05ffb0

" hi Identifier guifg=#c5c8c6
" hi Function guifg=#1dc967

" " #00aaff
hi Statement guifg=#00AAFF gui=NONE cterm=NONE
hi Conditional guifg=#dc59de
hi def link repeat Conditional
hi def link label Conditional
hi Operator guifg=#ec6965
hi Keyword guifg=#dc59de
hi def link Exception Conditional

hi PreProc guifg=#EF8D34
hi Include guibg=NONE
hi Macro guifg=NONE
hi PreCondit guifg=NONE

hi Type guifg=#835F5E gui=NONE cterm=NONE
" hi StorageClass guifg=#cccccc
" hi Structure guifg=#cccccc
" hi Typedef guifg=#cccccc

" hi Special guifg=#0000ff
hi SpecialChar guifg=#8352ff
" hi Tag guifg=#0000ff
hi Delimiter guifg=#c2c99c
" hi SpecialComment guifg=#ffff00
" hi Debug guifg=#0000ff

" }}}

" Go {{{

" Clear {{{
if has('nvim')
lua << EOS
    local groups = {
        '@boolean.go',
        '@comment.documentation.go',
        '@comment.go',
        '@conditional.go',
        '@constant.builtin.go',
        '@constant.go',
        '@constructor.go',
        '@error.go',
        '@field.go',
        '@float.go',
        '@type.builtin.go',
        '@function.call.go',
        '@function.go',
        '@include.go',
        '@keyword.coroutine.go',
        '@keyword.function.go',
        '@keyword.go',
        '@keyword.return.go',
        '@label.go',
        '@method.call.go',
        '@method.go',
        '@namespace.go',
        '@number.go',
        '@operator.go',
        '@parameter.go',
        '@property.go',
        '@punctuation.bracket.go',
        '@punctuation.delimiter.go',
        '@repeat.go',
        '@spell.go',
        '@string.esacpe.go',
        '@string.go',
        '@string.regex.go',
        '@type.builtin.go',
        '@type.definition.go',
        '@type.go',
        '@variable.go',
    }

    for _, v in ipairs(groups) do
        vim.cmd.highlight({v, "guifg=none guibg=none ctermfg=NONE ctermbg=NONE gui=none cterm=NONE"})
    end
EOS
end
" }}}

if has('nvim')
    hi @constant.go guifg=#00ccbb
    hi @boolean.go guifg=#00ccbb
    hi @number.go guifg=#00ccbb

    hi @label.go guifg=#ff0000

    hi @keyword.go guifg=#dc59de
    hi @conditional.go guifg=#dc59de
    hi @keyword.return.go guifg=#dc59de
    hi @keyword.coroutine.go guifg=#dc59de
    hi @keyword.function.go guifg=#1dc967

    hi @type.go guifg=#835F5E
    hi @type.builtin.go guifg=#8352ff
    hi @parameter.go guifg=#00ccbb
    hi @field.go guifg=#9385ff

    hi @method.go guifg=#ffffff
    hi @include.go guifg=#f64c8d

    hi @function.builtin.go guifg=#EF8D34
    hi @function.call.go guifg=#0baef4
    hi @method.call.go guifg=#0baef4

    hi @punctuation.bracket.go guifg=#c2c99c
    hi @punctuation.delimiter.go guifg=#c2c99c
    hi @operator.go guifg=#ec6965

    hi @string.go guifg=#C69669
    hi @string.escape.go guifg=#8352ff
else
    hi goPackage guifg=#f64c8d
    hi goImport guifg=#f64c8d
    hi goParen guifg=#c2c99c
    hi goBlock guifg=#c2c99c
    hi goType guifg=#835F5E
    hi goParamType guifg=#835F5E
    hi goDeclType guifg=#835F5E
    hi goStatement guifg=#dc59de
    hi goLabel guifg=#dc59de
    hi goDeclaration guifg=#1dc967
    hi goFunction guifg=#ffffff
    hi goTypeName guifg=#ffffff
    hi goBuiltins guifg=#EF8D34
    hi goReceiver guifg=#c2c99c
    hi goReceiverVar guifg=#00ccbb
    hi goPointerOperator guifg=#ec6965
    hi goOperator guifg=#ec6965
    hi goParamName guifg=#00ccbb
    hi goSimpleParams guifg=#c2c99c
    hi goFunctionCall guifg=#0baef4
    hi goConst guifg=#cd59de
end

" }}}

" NeoVim LSP {{{

hi DiagnosticVirtualTextError guifg=#ff1f53
hi DiagnosticVirtualTextWarn guifg=#ffbe3d

" }}}

" Vim LSP {{{

hi LspErrorHighlight guibg=#5E1317
hi LspWarningHighlight guibg=#3E3F07
hi LspInformationHighlight guibg=#631630
hi LspHintHighlight guibg=#2F2F2F

hi LspErrorVirtualText guifg=red
hi LspWarningVirtualText guifg=yellow
hi LspInformationVirtualText guifg=blue
hi LspHintVirtualText guifg=green gui=italic

hi lspInlayHintsType guifg=#585858
hi def link lspInlayHintsParameter lspInlayHintsType

" }
