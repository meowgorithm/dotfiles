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
hs.window.animationDuration = 0

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

-- Menu
-- Ported from @maaslalani's fennel version
-- https://github.com/maaslalani/_/blob/4b7844ef73846b0353fadb02650d79cbd34d47ca/fnl/hammerspoon.fnl
do
	local modalWidth = 20
	local mainModal = hs.hotkey.modal.new({ "ctrl" }, "space")

	local function launch(application)
		return function()
			return hs.application.launchOrFocus(application)
		end
	end

	local function openUrl(url)
		return function()
			return hs.urlevent.openURL(url)
		end
	end

	local menu = {
		t = { name = "Terminal", action = launch("Kitty") },
		b = { name = "Browser", action = launch("Arc") },
	}

	local function setupMenu(modal, menu)
		modal.exited = function()
			hs.alert.closeAll()
		end
		modal:bind({}, "escape", function()
			modal:exit()
		end)

		local readout = {}
		for k, v in pairs(menu) do
			if type(v) == "table" then
				local gap = string.rep(" ", (modalWidth - #v.name))
				readout[#readout + 1] = (k .. gap .. v.name)

				local action

				if v.action ~= nil then
					action = v.action
				else
					local subMenu = hs.hotkey.modal.new()
					setupMenu(subMenu, v)
					action = function()
						subMenu:enter()
					end
				end

				modal:bind({}, k, function()
					modal:exit()
					action()
				end)
			end
		end

		modal.entered = function()
			hs.alert.closeAll()
			hs.alert(table.concat(readout, "\n"), true)
		end
	end

	setupMenu(mainModal, menu)
end
