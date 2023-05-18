local g = vim.g
local opt = vim.opt
local keymap = vim.keymap
local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup
local clear_autocmds = vim.api.nvim_clear_autocmds
local hi = vim.api.nvim_set_hl

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

autocmd("FileType", { pattern = "lua", command = "set noexpandtab" })

-- Trim trailing whitespace on save
autocmd({ "BufWritePre" }, { pattern = { "*" }, command = [[%s/\s\+$//e]] })

-- Highlight current line in current window only
autocmd("WinEnter,BufEnter", { pattern = "*", command = "setlocal cursorline" })
autocmd("WinLeave", { pattern = "*", command = "setlocal nocursorline" })

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

-- GitGutter
g.gitgutter_sign_modified = "•"
hi(0, "GitGutterAdd", { fg = "#009900" })
hi(0, "GitGutterChange", { fg = "#bbbb00" })
hi(0, "GitGutterDelete", { fg = "#ff2222" })

-- LSP
do
	local lsp = require("lspconfig")
	local cmp = require("cmp")

	local capabilities = require("cmp_nvim_lsp").default_capabilities()

	local format = function(bufnr)
		vim.lsp.buf.format({
			filter = function(client)
				return client.name == "null-ls" -- promote null-ls
			end,
			bufnr = bufnr,
		})
	end

	local formatGroup = augroup("LspFormatting", {})

	local on_attach = function(client, bufnr)
		local map = function(key, cmd)
			vim.api.nvim_buf_set_keymap(bufnr, "n", key, "<cmd>lua " .. cmd .. "<cr>", {
				noremap = true,
				silent = true,
			})
		end
		map("gd", "vim.lsp.buf.definition()")
		map("K", "vim.lsp.buf.hover()")
		map("<leader>lr", "vim.lsp.buf.rename()")
		map("<leader>a", "vim.lsp.buf.code_action()")
		map("gl", "vim.diagnostic.open_float()")
		vim.cmd([[ command! Format execute 'lua vim.lsp.buf.format()' ]])
		if client.supports_method("textDocument/formatting") then
			clear_autocmds({ group = formatGroup, buffer = bufnr })
			autocmd("BufWritePre", {
				group = formatGroup,
				buffer = bufnr,
				callback = function()
					format(bufnr)
				end,
			})
		end
	end

	cmp.setup({
		snippet = {
			expand = function(args)
				vim.fn["vsnip#anonymous"](args.body)
			end,
		},
		window = {
			completion = cmp.config.window.bordered(),
			documentation = cmp.config.window.bordered(),
		},
		mapping = cmp.mapping.preset.insert({
			["<c-b>"] = cmp.mapping.scroll_docs(-4),
			["<c-f>"] = cmp.mapping.scroll_docs(4),
			["<C-Space>"] = cmp.mapping.complete(),
			["<tab>"] = cmp.mapping.complete(),
			["<c-e>"] = cmp.mapping.abort(),
			["<cr>"] = cmp.mapping.confirm({ select = true }),
		}),
		sources = cmp.config.sources({
			{ name = "nvim_lsp" },
			{ name = "vsnip" },
		}, { name = "buffer" }),
		experimental = {
			ghost_text = true,
		},
	})

	cmp.setup.cmdline({ "/", "?" }, {
		mapping = cmp.mapping.preset.cmdline(),
		sources = {
			{ name = "buffer" },
		},
	})

	cmp.setup.cmdline(":", {
		mapping = cmp.mapping.preset.cmdline(),
		sources = cmp.config.sources({
			{ name = "path" },
		}, {
			{ name = "cmdline" },
		}),
	})

	lsp.bashls.setup({
		capabilities = capabilities,
		on_attach = on_attach,
	})

	lsp.nil_ls.setup({
		capabilities = capabilities,
		on_attach = on_attach,
	})

	lsp.hls.setup({
		capabilities = capabilities,
		on_attach = on_attach,
	})

	lsp.golangci_lint_ls.setup({
		on_attach = on_attach,
		capabilities = capabilities,
	})

	lsp.gopls.setup({
		on_attach = on_attach,
		capabilities = capabilities,
	})

	lsp.lua_ls.setup({
		on_attach = on_attach,
		capabilities = capabilities,
		settings = {
			Lua = {
				diagnostics = { globals = { "vim" } },
			},
		},
	})

	lsp.jsonls.setup({
		capabilities = capabilities,
		on_attach = on_attach,
	})

	lsp.tsserver.setup({
		capabilities = capabilities,
		on_attach = on_attach,
	})

	lsp.html.setup({
		capabilities = capabilities,
		on_attach = on_attach,
	})

	lsp.yamlls.setup({
		capabilities = capabilities,
		on_attach = on_attach,
	})

	local null_ls = require("null-ls")
	null_ls.setup({
		sources = {
			null_ls.builtins.formatting.alejandra,
			null_ls.builtins.formatting.stylua,
			null_ls.builtins.formatting.shfmt,
			null_ls.builtins.formatting.goimports,
		},
		capabilities = capabilities,
		on_attach = on_attach,
	})
end