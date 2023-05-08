local g = vim.g
local opt = vim.opt
local keymap = vim.keymap
local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

g.mapleader = " "

local options = {
	compatible = false,
	backup = false,
	writebackup = false,
	swapfile = false,
	shell = "bash",
	number = true,
	wrap = false,
	expandtab = true,
	shiftwidth = 4,
	tabstop = 4,
	softtabstop = 4,
	termguicolors = true,
	showmatch = true,
	colorcolumn = "80",
	mouse = "a",
	spell = false,
	spelllang = { "en_us" },
	cursorline = true,
	ruler = true,
	signcolumn = "yes",
	listchars = "tab:▸ ,trail:·,eol:¶",
	splitbelow = true,
	splitright = true,
	-- Search
	hlsearch = false,
	ignorecase = true,
	incsearch = true,
	wrapscan = true,
	-- Persistent undo
	undofile = true,
	undodir = vim.fn.stdpath("data") .. "/undo",
}

for k, v in pairs(options) do
	opt[k] = v
end

-- Visual Mode Blockwise Indent. This keeps the current visual block selection
-- active after changing indent with '<' or '>'. Usually the visual block
-- selection is lost after you shift it, which is incredibly annoying.
--
-- http://vim.wikia.com/wiki/Short_mappings_for_common_tasks
keymap.set("v", ">", ">gv")
keymap.set("v", "<", "<gv")

keymap.set("n", "S", ":split<CR>")
keymap.set("n", "VS", ":vsplit<CR>")
keymap.set("n", "<leader>i", ":set invlist<CR>")
keymap.set("n", "<leader>s", ":set hlsearch! hlsearch?<CR>")
keymap.set("n", "<leader>w", ":set wrap! wrap?<CR>")
keymap.set("n", "E", ":TroubleToggle<CR>")

autocmd("FileType", {
	pattern = "lua",
	command = "set noexpandtab",
})

-- Highlight current line in current window only
autocmd("WinEnter,BufEnter", { pattern = "*", command = "setlocal cursorline" })
autocmd("WinLeave", { pattern = "*", command = "setlocal nocursorline" })

-- Format on save.
autocmd("BufWritePre", { pattern = "*", command = "lua vim.lsp.buf.format()" })

-- Markdown spell checking
augroup("markdownSpell", { clear = true })
autocmd("FileType", {
	pattern = "markdown",
	command = "setlocal spell",
	group = "markdownSpell",
})
autocmd("BufRead,BufNewFile", {
	pattern = "*.md",
	command = "setlocal spell",
	group = "markdownSpell",
})

-- CtrlP
if vim.fn.executable("rg") then
	opt.grepprg = "rg --color=never"
	g.ctrlp_user_command = 'rg %s --files --color=never --glob ""'
	g.ctrlp_use_caching = 0
end
g.ctrlp_max_height = 25
g.ctrlp_jump_to_buffer = 0 -- enable this to jump to open windows if the file is open there. see ctrlp help.
g.ctrlp_working_path_mode = "ra" -- try and find the repo root and search from there
keymap.set("n", ";", ":CtrlPBuffer<cr>")

-- Trouble
require("trouble").setup({
	icons = false,
	fold_open = "▼",
	fold_closed = "▶",
	indent_lines = false,
	signs = {
		error = "error",
		warning = "warn",
		hint = "hint",
		information = "info",
	},
	use_diagnostic_signs = false,
})
