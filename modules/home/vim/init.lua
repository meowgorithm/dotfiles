local g = vim.g
local opt = vim.opt
local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup
local clear_autocmds = vim.api.nvim_clear_autocmds
local hi = vim.api.nvim_set_hl

local nmap = function(lhs, rhs, opts)
	vim.keymap.set("n", lhs, rhs, opts)
end
local vmap = function(lhs, rhs, opts)
	vim.keymap.set("v", lhs, rhs, opts)
end
local imap = function(lhs, rhs, opts)
	vim.keymap.set("i", lhs, rhs, opts)
end

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
	autoread = true,
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
	scrolloff = 8,
	foldmethod = "marker",
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

nmap("gs", "^")
nmap("gl", "$")
nmap("S", "<cmd>split<cr>")
nmap("VS", "<cmd>vsplit<cr>")
nmap("gn", "<cmd>bnext<cr>")
nmap("gp", "<cmd>bprev<cr>")
nmap("<leader>f", "<cmd>Telescope find_files<cr>")
nmap("<leader>g", "<cmd>Telescope live_grep<cr>")
nmap(";", "<cmd>Telescope buffers<cr>")

-- Visual Mode Blockwise Indent. This keeps the current visual block selection
-- active after changing indent with '<' or '>'. Usually the visual block
-- selection is lost after you shift it, which is incredibly annoying.
--
-- http://vim.wikia.com/wiki/Short_mappings_for_common_tasks
vmap(">", ">gv")
vmap("<", "<gv")

-- How about if just one < or > indents in normal mode?
nmap(">", ">>")
nmap("<", "<<")

-- Toggles
nmap("<leader>i", "<cmd>set invlist<cr>")
nmap("<leader>s", "<cmd>set hlsearch! hlsearch?<cr>")
nmap("<leader>w", "<cmd>set wrap! wrap?<cr>")

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

-- Commentary
nmap("<c-c>", "<cmd>Commentary<cr>")

-- Tree
nmap("<leader>n", "<cmd>NvimTreeToggle<cr>")
require("nvim-tree").setup({
	disable_netrw = true,
	renderer = {
		icons = {
			symlink_arrow = " >> ",
			show = {
				git = true,
				folder = true,
				file = false,
				folder_arrow = false,
			},
			glyphs = {
				folder = {
					arrow_open = "",
					arrow_closed = "",
					default = "▶",
					open = "▼",
					empty = "▼",
					empty_open = "▼",
					symlink = "*",
					symlink_open = "!",
				},
				git = {
					unstaged = "!",
					staged = "✓",
					unmerged = "U",
					renamed = "➜",
					untracked = "*",
					deleted = "✗",
					ignored = "◌",
				},
			},
		},
		indent_markers = {
			enable = true,
			icons = {
				corner = "╰ ",
				edge = "│ ",
				none = "  ",
			},
		},
	},
})

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
nmap("E", "<cmd>TroubleToggle<cr>")

-- GitGutter
g.gitgutter_sign_modified = "•"
hi(0, "GitGutterAdd", { fg = "#009900" })
hi(0, "GitGutterChange", { fg = "#bbbb00" })
hi(0, "GitGutterDelete", { fg = "#ff2222" })

-- Color Picker
do
	local opts = { noremap = true, silent = true }
	nmap("<c-p>", "<cmd>PickColor<cr>", opts)
	-- imap("pci", "<cmd>PickColorInsert<cr>", opts)

	-- nmap("n", "your_keymap", "<cmd>ConvertHEXandRGB<cr>", opts)
	nmap("cv", "<cmd>ConvertHEXandHSL<cr>", opts)

	require("color-picker").setup({
		["icons"] = { "#", "|" },
		["border"] = "rounded",
		["keymap"] = {
			["U"] = "<Plug>ColorPickerSlider5Decrease",
			["O"] = "<Plug>ColorPickerSlider5Increase",
		},
		["background_highlight_group"] = "Normal",
		["border_highlight_group"] = "FloatBorder",
		["text_highlight_group"] = "Normal",
	})
end

-- Colorizer
-- Note: at the moment, Nix uses fork https://github.com/nvchad/nvim-colorizer.lua/
do
	-- :ShowColors
	vim.api.nvim_create_user_command("ShowColors", function(opts)
		require("colorizer").attach_to_buffer(0, {
			mode = opts.fargs[1],
			css = true,
		})
	end, {
		nargs = "?",
		complete = function(_, _, _)
			return { "virtualtext", "background" }
		end,
	})

	-- :HideColors
	vim.api.nvim_create_user_command("HideColors", function(_)
		require("colorizer").detach_from_buffer(0)
	end, { nargs = 0 })
end

-- TreeSitter
require("nvim-treesitter.configs").setup({
	highlight = { enable = true },
	indent = { enable = true },
	additional_vim_regex_highlighting = false,
})

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
		map("<leader>gl", "vim.diagnostic.open_float()")
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
			["<c-space>"] = cmp.mapping.complete(),
			--["<tab>"] = cmp.mapping.complete(),
			["<c-e>"] = cmp.mapping.abort(),
			["<cr>"] = cmp.mapping.confirm({ select = true }),
		}),
		sources = cmp.config.sources({
			{ name = "nvim_lsp" },
			{ name = "vsnip" },
		}, { name = "buffer" }),
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
		settings = {
			gopls = {
				gofumpt = true,
				codelenses = {
					gc_details = false,
					generate = true,
					regenerate_cgo = true,
					run_govulncheck = true,
					test = true,
					tidy = true,
					upgrade_dependency = true,
					vendor = true,
				},
				hints = {
					assignVariableTypes = true,
					compositeLiteralFields = true,
					compositeLiteralTypes = true,
					constantValues = true,
					functionTypeParameters = true,
					parameterNames = true,
					rangeVariableTypes = true,
				},
				analyses = {
					nilness = true,
					shadow = true,
					shift = true,
					unusedparams = true,
					unusedwrite = true,
					useany = true,
					unusedvariable = true,
				},
				usePlaceholders = true,
				completeUnimported = true,
				staticcheck = true,
				directoryFilters = { "-.git", "-node_modules" },
				semanticTokens = true,
			},
		},
	})

	lsp.lua_ls.setup({
		on_attach = on_attach,
		capabilities = capabilities,
		settings = {
			Lua = {
				diagnostics = { globals = { "vim", "hs" } },
			},
			completion = {
				callSnippet = "Replace",
			},
			hint = {
				enable = true,
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
			null_ls.builtins.formatting.prettier,
		},
		capabilities = capabilities,
		on_attach = on_attach,
	})
end
