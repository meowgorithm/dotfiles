-- Set up nvim-cmp.
local lsp = require("lspconfig")
local cmp = require("cmp")

local capabilities = require("cmp_nvim_lsp").default_capabilities()
local on_attach = function(client, bufnr) end

lsp.bashls.setup({
	capabilities = capabilities,
	on_attach = on_attach,
})

lsp.nil_ls.setup({
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
	},
	capabilities = capabilities,
	on_attach = on_attach,
})
