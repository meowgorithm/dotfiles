local hotkey = require("hs.hotkey")
local fzy = require("fzy")

-- Alerts
hs.alert.defaultStyle.fillColor = { hex = "#151618" }
hs.alert.defaultStyle.padding = 30
hs.alert.defaultStyle.radius = 8
hs.alert.defaultStyle.strokeColor = { hex = "#4F5053" }
hs.alert.defaultStyle.strokeWidth = 0
hs.alert.defaultStyle.textStyle = {
	font = { name = "JetBrains Mono", size = 14, color = { hex = "#F2F2F2" } },
	paragraphStyle = { lineHeightMultiple = 1.4 },
}
hs.alert.show("Hammerspoon Loaded", 2)

-- Disable animations
hs.window.animationDuration = 0

-- Window management
do
	local halfModifiers = { "alt", "ctrl", "cmd" }
	local quarterModifiers = { "shift", "alt", "ctrl" }

	-- Left
	hotkey.bind(halfModifiers, "left", function()
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
	hotkey.bind(halfModifiers, "right", function()
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

	-- Top
	hotkey.bind(halfModifiers, "up", function()
		local win = hs.window.focusedWindow()
		local f = win:frame()
		local screen = win:screen()
		local max = screen:frame()

		f.x = max.x
		f.y = max.y
		f.w = max.w
		f.h = max.h / 2
		win:setFrame(f)
	end)

	-- Bottom
	hotkey.bind(halfModifiers, "down", function()
		local win = hs.window.focusedWindow()
		local f = win:frame()
		local screen = win:screen()
		local max = screen:frame()

		f.x = max.x
		f.y = max.y + (max.h / 2)
		f.w = max.w
		f.h = max.h / 2
		win:setFrame(f)
	end)

	-- Top Left
	hotkey.bind(quarterModifiers, "left", function()
		local win = hs.window.focusedWindow()
		local f = win:frame()
		local screen = win:screen()
		local max = screen:frame()

		f.x = max.x
		f.y = max.y
		f.w = max.w / 2
		f.h = max.h / 2
		win:setFrame(f)
	end)

	-- Bottom Left
	hotkey.bind(quarterModifiers, "down", function()
		local win = hs.window.focusedWindow()
		local f = win:frame()
		local screen = win:screen()
		local max = screen:frame()

		f.x = max.x
		f.y = max.y + (max.h / 2)
		f.w = max.w / 2
		f.h = max.h / 2
		win:setFrame(f)
	end)

	-- Top Right
	hotkey.bind(quarterModifiers, "up", function()
		local win = hs.window.focusedWindow()
		local f = win:frame()
		local screen = win:screen()
		local max = screen:frame()

		f.x = max.x + (max.w / 2)
		f.y = max.y
		f.w = max.w / 2
		f.h = max.h / 2
		win:setFrame(f)
	end)

	-- Bottom Right
	hotkey.bind(quarterModifiers, "right", function()
		local win = hs.window.focusedWindow()
		local f = win:frame()
		local screen = win:screen()
		local max = screen:frame()

		f.x = max.x + (max.w / 2)
		f.y = max.y + (max.h / 2)
		f.w = max.w / 2
		f.h = max.h / 2
		win:setFrame(f)
	end)

	-- Center
	hotkey.bind(halfModifiers, "C", function()
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
	hotkey.bind(halfModifiers, "M", function()
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

-- App/URL/Folder Chooser
do
	local bin = "~/.nix-profile/bin/"

	local function gpgDecrypt(path)
		local handle = assert(io.popen(bin .. "gpg --quiet --decrypt --no-tty " .. path .. " 2> /dev/null"))
		return handle:read("*all")
	end

	local src = gpgDecrypt("~/.hammerspoon/rc.lua.gpg")
	local config = load(src)()

	-- Returns whether or not the system has dark mode enabled.
	local function darkModeIsActive()
		local handle = assert(io.popen("defaults read -g AppleInterfaceStyle"))
		local res = handle:read("*all")
		if string.find(res, "Dark") then
			return true
		end
		return false
	end

	-- Set dark mode on the system and chooser.
	local function setDarkMode(v, chooser)
		local fmt <const> = "Application('System Events').appearancePreferences.darkMode.set(%s)"
		hs.osascript.javascript(string.format(fmt, v))
	end

	-- Toggle dark mode.
	local function toggleDarkMode()
		setDarkMode(not darkModeIsActive())
	end

	local function trim(str)
		return string.gsub(str, "^%s*(.-)%s*$", "%1")
	end

	local function filenameWithoutExtension(str)
		return str:match("(.+)%..+")
	end

	local function getExtension(str)
		return str:match("^.+(%..+)$")
	end

	local excludes = {
		".",
		"..",
		"Adobe After Effects Render Engine 2025.app",
		"Audio Midi Setup",
		"Automator",
		"Boot Camp Assistant.app",
		"Chess",
		"Cinema 4D Team Render Client.app",
		"Cinema 4D Team Render Server.app",
		"Cineware.app",
		"Commandline.app", -- a C4D thing
		"Digital Color Meter",
		"Freeform.app",
		"Grapher.app",
		"Launchpad",
		"Migration Assistant.app",
		"News",
		"Podcasts",
		"Red Giant FxPlug",
		"Screen Sharing",
		"Screenshot",
		"Siri",
		"TV",
		"Voiceover Utility",
		"Wacom Center",
		"Wacom Display Settings",
		"Wacom Tablet Utility",
		"c4dpy.app",
		"redshift",
	}
	for _, v in ipairs(config.excludes) do
		table.insert(excludes, v)
	end

	local function exclude(file)
		for _, v in ipairs(excludes) do
			if v == file then
				return true
			end
		end
		return false
	end

	local function shallowTableMerge(a, b)
		for _, v in pairs(b) do
			table.insert(a, v)
		end
	end

	local maxDepth <const> = 1

	local function findApps(dir, depth)
		local choices = {}
		local attrs, path
		for file in hs.fs.dir(dir) do
			path = dir .. "/" .. file

			if exclude(file) then
				goto continue
			elseif getExtension(file) == ".app" then
				table.insert(choices, {
					text = filenameWithoutExtension(file),
					image = hs.image.iconForFile(path),
					type = "app",
				})
				goto continue
			end

			attrs = hs.fs.attributes(path)

			if attrs["mode"] == "directory" and depth < maxDepth then
				local subChoices = findApps(path, depth + 1)
				shallowTableMerge(choices, subChoices)
			end

			::continue::
		end
		return choices
	end

	local choices = {}
	shallowTableMerge(choices, findApps("/Applications", 0))
	shallowTableMerge(choices, findApps("/System/Applications", 0))
	shallowTableMerge(choices, findApps("/Users/christian/Applications", 0))

	local function insertURL(t, text, subText)
		table.insert(t, {
			text = text,
			subText = subText,
			image = hs.image.imageFromAppBundle("com.apple.Safari"),
			type = "url",
		})
	end

	local function insertFolder(t, text, subText)
		table.insert(t, {
			text = text,
			subText = subText,
			image = hs.image.imageFromAppBundle("com.apple.Finder"),
			type = "folder",
		})
	end

	insertURL(choices, "Charm", "https://charm.sh")
	insertURL(choices, "Stars", "https://charm.sh/stars/")

	table.insert(choices, {
		text = "Reload Hammerspoon",
		image = hs.image.imageFromAppBundle("com.apple.ScriptEditor2"),
		type = "reload",
	})

	table.insert(choices, {
		text = "Toggle Dark Mode",
		image = hs.image.imageFromAppBundle("com.apple.systempreferences"),
		type = "toggle-dark-mode",
	})

	for _, v in ipairs(config.choices) do
		if v.type then
			if v.type == "url" then
				insertURL(choices, v.text, v.subText)
			elseif v.type == "folder" then
				insertFolder(choices, v.text, v.subText)
			end
			goto continue
		end
		print("Skipping choice: " .. v.text .. " (unknown or missing type)")
		::continue::
	end

	table.sort(choices, function(a, b)
		return a.text < b.text
	end)

	local chooser = hs.chooser.new(function(choice)
		if not choice then
			return
		end
		if choice.type == "app" then
			hs.application.launchOrFocus(choice.text)
		elseif choice.type == "url" then
			hs.urlevent.openURL(choice.subText)
		elseif choice.type == "folder" then
			hs.execute("open " .. choice.subText)
		elseif choice.type == "reload" then
			hs.reload()
		elseif choice.type == "toggle-dark-mode" then
			toggleDarkMode()
		else
			hs.pasteboard.setContents(choice.text)
			hs.alert.show("Copied " .. choice.text)
		end
	end)

	chooser:queryChangedCallback(function(str)
		-- Filter manually. Normally if chooser:queryChangedCallback is
		-- undefined Hammerspoon will filter automatically.
		--
		-- We're using a fuzzy search algorithm here
		-- (https://github.com/swarn/fzy-lua) however we could also just match
		-- with string.find if we didn't want to rely on an external library.
		local newChoices = {}
		for _, v in ipairs(choices) do
			if fzy.has_match(str, v.text) then
				v.score = fzy.score(str, v.text)
				table.insert(newChoices, v)
			end
		end

		if #newChoices > 0 then
			table.sort(newChoices, function(a, b)
				return a.score > b.score
			end)
			chooser:choices(newChoices)
			return
		end

		-- If no matches pass to bc as an arithmetic expression.
		local res, ok, _, _ = hs.execute("echo " .. str .. " | bc -l")
		if ok then
			table.insert(newChoices, {
				text = trim(res),
				image = hs.image.imageFromAppBundle("com.apple.Calculator"),
				type = "copy",
			})
			chooser:choices(newChoices)
		end
	end)

	chooser:choices(choices)
	chooser:searchSubText(false)
	chooser:rows(10)
	chooser:width(25)
	chooser:bgDark(darkModeIsActive())
	hs.hotkey.bind({ "cmd" }, "space", function()
		chooser:show()
	end)
end
