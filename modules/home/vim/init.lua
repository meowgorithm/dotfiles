local g = vim.g
local opt = vim.opt
local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup
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
	sidescrolloff = 5,
	foldmethod = "marker",
	equalalways = true,
	conceallevel = 0,
	-- Search
	hlsearch = false,
	ignorecase = true,
	incsearch = true,
	wrapscan = true,
	-- Persistent undo
	undofile = true,
	undodir = vim.fn.stdpath("data") .. "/undo",
	-- Text foramtting options. See :help fo-table.
	formatoptions = "tcroq1]jp",
}

for k, v in pairs(options) do
	opt[k] = v
end

nmap("gs", "^")
nmap("gl", "$")
nmap("gn", "<cmd>bnext<cr>")
nmap("gp", "<cmd>bprev<cr>")
nmap("BO", "<cmd>BufOnly<cr>")
nmap("<leader>f", "<cmd>Telescope find_files<cr>")
nmap("<leader>g", "<cmd>Telescope live_grep<cr>")
nmap("<leader>sd", "<cmd>Telescope lsp_document_symbols<cr>")
nmap("<leader>sw", "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>")
nmap("<leader>r", "<cmd>Telescope registers<cr>")
nmap(";", "<cmd>Telescope buffers<cr>")
nmap("<leader>w", "<cmd>write<cr>")
nmap("<leader>n", "<cmd>noa write<cr>")
nmap("<c-c>", "gcc")
nmap("<leader>y", '"+y')

-- Visual Mode Blockwise Indent. This keeps the current visual block selection
-- active after changing indent with '<' or '>'. Usually the visual block
-- selection is lost after you shift it, which is incredibly annoying.
--
-- http://vim.wikia.com/wiki/Short_mappings_for_common_tasks
vmap(">", ">gv")
vmap("<", "<gv")

-- And how about if just one < or > indents in normal mode as well?
nmap(">", ">>")
nmap("<", "<<")

-- Toggles
nmap("<leader>i", "<cmd>set invlist<cr>")
nmap("<leader>S", "<cmd>set hlsearch! hlsearch?<cr>")
nmap("<leader>W", "<cmd>set wrap! wrap?<cr>")

autocmd({ "FileType" }, { pattern = "lua", command = "set noexpandtab" })

-- Trim trailing whitespace on save
autocmd({ "BufWritePre" }, { pattern = { "*" }, command = [[%s/\s\+$//e]] })

-- Highlight current line in current window only
autocmd({ "WinEnter", "BufEnter" }, { pattern = "*", command = "setlocal cursorline" })
autocmd({ "WinLeave" }, { pattern = "*", command = "setlocal nocursorline" })

-- Keep splits equal size
autocmd("VimResized", { pattern = "*", command = "wincmd =" })

-- Markdown spell checking
augroup("markdownSpell", { clear = true })
autocmd({ "FileType" }, {
	pattern = "markdown",
	command = "setlocal spell",
	group = "markdownSpell",
})
autocmd({ "BufRead", "BufNewFile" }, {
	pattern = "*.md",
	command = "setlocal spell",
	group = "markdownSpell",
})

-- Colorizer
require("colorizer").setup({ names = false })

-- Smear
require("smear_cursor").enabled = false

-- Tree
nmap("<leader>t", "<cmd>NvimTreeToggle<cr>")
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

-- Telescope
require("telescope").setup({
	defaults = {
		mappings = {
			i = { ["<c-d>"] = require("telescope.actions").delete_buffer },
		},
	},
})

-- Trouble
require("trouble").setup()
nmap("E", "<cmd>Trouble diagnostics toggle<cr>")
nmap("<leader>xx", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>")

-- GitGutter
g.gitgutter_sign_modified = "•"
hi(0, "GitGutterAdd", { fg = "#009900" })
hi(0, "GitGutterChange", { fg = "#bbbb00" })
hi(0, "GitGutterDelete", { fg = "#ff2222" })

-- Avante
require("avante_lib").load()
require("avante").setup({
	behaviour = { auto_suggestions = false },
	hints = { enabled = true },
	windows = {
		width = 30,
		sidebar_header = {
			align = "left",
			rounded = true,
		},
	},
})

-- TreeSitter
require("nvim-treesitter.configs").setup({
	highlight = { enable = true, additional_vim_regex_highlighting = false },
	indent = { enable = true },
})

-- Vsnip
imap("<c-l>", "vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : '<c-l>'", { expr = true })

-- Blink
require("blink.cmp").setup({
	keymap = {
		preset = "default",
		cmdline = { preset = "default" },
	},
	completion = {
		list = {
			selection = {
				preselect = true,
				auto_insert = true,
			},
		},
		menu = {
			border = "none", -- "rounded" is the other one we like

			-- Don't show completion menu automatically when searching
			auto_show = function(ctx)
				return ctx.mode ~= "cmdline" or not vim.tbl_contains({ "/", "?" }, vim.fn.getcmdtype())
			end,

			-- Don't show completion automatically in cmdline mode
			-- auto_show = function(ctx) return ctx.mode ~= "cmdline" end,
		},
		documentation = {
			auto_show = true,
			auto_show_delay_ms = 250,
			treesitter_highlighting = true,
		},
	},
	sources = {
		providers = {
			lsp = {
				min_keyword_length = 1,
				score_offset = 0,
			},
			path = {
				min_keyword_length = 0,
			},
			buffer = {
				min_keyword_length = 1,
				max_items = 5,
			},
		},
	},
})

-- Conform
do
	require("conform").setup({
		formatters_by_ft = {
			cabal = { "cabal_fmt" },
			css = { "prettier" },
			haskell = { "fourmolu" },
			html = { "prettier" },
			javascript = { "prettier" },
			json = { "jq" },
			lua = { "stylua" },
			markdown = { "prettier" },
			nix = { "alejandra" },
			sh = { "shfmt" },
			yaml = { "prettier" },
			-- ["_"] = { "trim_whitespace", "trim_newlines" },
		},
		format_on_save = {
			timeout_ms = 500,
			lsp_format = "fallback",
		},
	})

	vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
end

-- LSP
do
	local lsp = require("lspconfig")
	local lspMethods = vim.lsp.protocol.Methods

	require("lspconfig.ui.windows").default_options.border = "rounded"

	local capabilities = require("blink.cmp").get_lsp_capabilities({
		workspace = {
			didChangeWatchedFiles = {
				dynamicRegistration = true, -- needs fswatch on linux
				relativePatternSupport = true,
			},
		},
	}, true)

	local on_attach = function(client, bufnr)
		local map = function(key, cmd)
			vim.api.nvim_buf_set_keymap(bufnr, "n", key, "<cmd>lua " .. cmd .. "<cr>", {
				noremap = true,
				silent = true,
			})
		end

		map("gd", "vim.lsp.buf.definition()")
		map("<leader>lr", "vim.lsp.buf.rename()")
		map("<leader>a", "vim.lsp.buf.code_action()")
		map("<leader>ll", "vim.diagnostic.open_float()")

		if client.supports_method(lspMethods.textDocument_codelens) then
			map("<leader>lh", "vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())")
		end

		-- Organize imports before save
		if client.supports_method(lspMethods.textDocument_codeAction) then
			if client.name ~= "lua_ls" then
				-- Organize imports routine
				local oraganize_imports = function()
					local params = vim.lsp.util.make_range_params()
					local timeoutms = 1000
					params.context = { only = { "source.organizeImports" } }
					local result = client.request_sync(lspMethods.textDocument_codeAction, params, timeoutms, bufnr)
						or {}
					for _, r in pairs(result.result or {}) do
						if r.edit then
							local enc = client.offset_encoding or "utf-16"
							vim.lsp.util.apply_workspace_edit(r.edit, enc)
						elseif r.command and r.command.command then
							vim.lsp.buf.execute_command(r.command)
						end
					end
				end

				-- Group name for autocmd
				local lspAttachGroup = vim.api.nvim_create_augroup("LspAttachGroup", { clear = true })

				-- Create autocmd
				vim.api.nvim_create_autocmd({ "BufWritePre" }, {
					buffer = bufnr,
					callback = oraganize_imports,
					group = lspAttachGroup,
				})
			end
		end
	end

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
					gc_details = true,
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

	lsp.ts_ls.setup({
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
end
