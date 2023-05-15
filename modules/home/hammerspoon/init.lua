local hotkey = require("hs.hotkey")

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
