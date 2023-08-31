local wezterm = require("wezterm")

local config = {}

if wezterm.config_builder then
	config = wezterm.config_builder()
end

config.font = wezterm.font({
	family = "JetBrains Mono",
	weight = "Medium",
})
config.font_size = 13.0
config.line_height = 1.0
config.cell_width = 0.90
config.hide_tab_bar_if_only_one_tab = true
config.window_decorations = "RESIZE"
config.color_scheme = "Pantera Negra"
config.window_padding = {
	left = "16pt",
	right = "16pt",
	top = "12pt",
	bottom = "12pt",
}

return config
