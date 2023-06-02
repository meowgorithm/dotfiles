hs.loadSpoon("SpoonInstall")

local hotkey = require("hs.hotkey")

-- Alerts
hs.alert.defaultStyle.fillColor = { hex = "#151618" }
hs.alert.defaultStyle.padding = 30
hs.alert.defaultStyle.radius = 8
hs.alert.defaultStyle.strokeColor = { hex = "#4F5053" }
hs.alert.defaultStyle.strokeWidth = 0
hs.alert.defaultStyle.textStyle = {
	font = { name = "SF Mono Regular", size = 14, color = { hex = "#F2F2F2" } },
	paragraphStyle = { lineHeightMultiple = 1.4 },
}
hs.alert.show("Hammerspoon Loaded", 2)

-- Disable animations
hs.window.animationDuration = 0

-- Window management
do
	local modifiers = { "alt", "ctrl", "cmd" }

	-- Left
	hotkey.bind(modifiers, "left", function()
		local win = hs.window.focusedWindow()
		local f = win:frame()
		local screen = win:screen()
		local max = screen:frame()

		f.x = max.x
		f.y = max.y
		f.w = max.w / 2
		f.h = max.h
		win:setFrame(f)
	end)

	-- Right
	hotkey.bind(modifiers, "right", function()
		local win = hs.window.focusedWindow()
		local f = win:frame()
		local screen = win:screen()
		local max = screen:frame()

		f.x = max.x + (max.w / 2)
		f.y = max.y
		f.w = max.w / 2
		f.h = max.h
		win:setFrame(f)
	end)

	-- Center
	hotkey.bind(modifiers, "C", function()
		local win = hs.window.focusedWindow()
		local f = win:frame()
		local max = win:screen():frame()

		f.x = max.x * 0.8
		f.y = max.y * 0.8
		f.w = max.w * 0.8
		f.h = max.h * 0.8

		win:setFrame(f)
		win:centerOnScreen()
	end)

	-- Maximize
	hotkey.bind(modifiers, "M", function()
		local win = hs.window.focusedWindow()
		local f = win:frame()
		local max = win:screen():frame()

		f.x = max.x
		f.y = max.y
		f.w = max.w
		f.h = max.h

		win:setFrame(f)
	end)
end
