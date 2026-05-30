-- This is an example Hyprland Lua config file.
-- Refer to the wiki for more information.
-- https://wiki.hypr.land/Configuring/Start/

-- Please note not all available settings / options are set here.
-- For a full list, see the wiki

-- You can (and should!!) split this configuration into multiple files
-- Create your files separately and then require them like this:
-- require("myColors")


------------------
---- MONITORS ----
------------------

-- See https://wiki.hypr.land/Configuring/Basics/Monitors/
-- eDP-1 (laptop) on the left, DP-2 (LG external 4K) primary on the right.
hl.monitor({
    output   = "eDP-1",
    mode     = "preferred",
    position = "-1600x0",
    scale    = "1.6",
})
hl.monitor({
    output   = "DP-2",
    mode     = "preferred",
    position = "0x0",
    scale    = "1.25",
})


---------------------
---- MY PROGRAMS ----
---------------------

-- Set programs that you use
local terminal    = "kitty"
local fileManager = "dolphin"
local menu        = "hyprlauncher"


-------------------
---- AUTOSTART ----
-------------------

-- See https://wiki.hypr.land/Configuring/Basics/Autostart/

-- Autostart necessary processes (like notifications daemons, status bars, etc.)
-- Or execute your favorite apps at launch like this:
--
hl.on("hyprland.start", function ()
    hl.exec_cmd("swaybg -c '#3A3943'")
    hl.exec_cmd("quickshell")
    hl.exec_cmd("wl-paste --type text  --watch cliphist store")
    hl.exec_cmd("wl-paste --type image --watch cliphist store")
    hl.exec_cmd("ssh-agent -a " .. os.getenv("XDG_RUNTIME_DIR") .. "/ssh-agent.socket")
end)

local function restartQuickshell()
    hl.exec_cmd("sh -c 'sleep 1; restart-quickshell'")
end

hl.on("monitor.added", restartQuickshell)
hl.on("monitor.removed", restartQuickshell)


-------------------------------
---- ENVIRONMENT VARIABLES ----
-------------------------------

-- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Environment-variables/

hl.env("XCURSOR_SIZE", "24")
hl.env("HYPRCURSOR_SIZE", "24")
hl.env("SSH_AUTH_SOCK", os.getenv("XDG_RUNTIME_DIR") .. "/ssh-agent.socket")


-----------------------
----- PERMISSIONS -----
-----------------------

-- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Permissions/
-- Please note permission changes here require a Hyprland restart and are not applied on-the-fly
-- for security reasons

-- hl.config({
--   ecosystem = {
--     enforce_permissions = true,
--   },
-- })

-- hl.permission("/usr/(bin|local/bin)/grim", "screencopy", "allow")
-- hl.permission("/usr/(lib|libexec|lib64)/xdg-desktop-portal-hyprland", "screencopy", "allow")
-- hl.permission("/usr/(bin|local/bin)/hyprpm", "plugin", "allow")


-----------------------
---- LOOK AND FEEL ----
-----------------------

-- Refer to https://wiki.hypr.land/Configuring/Basics/Variables/
hl.config({
    general = {
        gaps_in  = 5,
        gaps_out = { top = 8, right = 10, bottom = 10, left = 10 },

        border_size = 2,

        col = {
            active_border   = { colors = {"rgba(33ccffee)", "rgba(00ff99ee)"}, angle = 45 },
            inactive_border = "rgba(595959aa)",
        },

        -- Set to true to enable resizing windows by clicking and dragging on borders and gaps
        resize_on_border = false,

        -- Please see https://wiki.hypr.land/Configuring/Advanced-and-Cool/Tearing/ before you turn this on
        allow_tearing = false,

        layout = "dwindle",
    },

    decoration = {
        rounding       = 10,
        rounding_power = 2,

        -- Change transparency of focused and unfocused windows
        active_opacity   = 1.0,
        inactive_opacity = 1.0,

        shadow = {
            enabled      = false,
            range        = 4,
            render_power = 9,
            color        = 0xee1a1a1a,
        },

        blur = {
            enabled   = true,
            size      = 3,
            passes    = 1,
            vibrancy  = 0.1696,
        },
    },

    animations = {
        enabled = true,
    },
})

-- Default curves and animations, see https://wiki.hypr.land/Configuring/Advanced-and-Cool/Animations/
hl.curve("easeOutQuint",   { type = "bezier", points = { {0.23, 1},    {0.32, 1}    } })
hl.curve("easeInOutCubic", { type = "bezier", points = { {0.65, 0.05}, {0.36, 1}    } })
hl.curve("linear",         { type = "bezier", points = { {0, 0},       {1, 1}       } })
hl.curve("almostLinear",   { type = "bezier", points = { {0.5, 0.5},   {0.75, 1}    } })
hl.curve("quick",          { type = "bezier", points = { {0.15, 0},    {0.1, 1}     } })

-- Default springs
hl.curve("easy",           { type = "spring", mass = 1, stiffness = 71.2633, dampening = 15.8273644 })

hl.animation({ leaf = "global",        enabled = true,  speed = 10,   bezier = "default" })
hl.animation({ leaf = "border",        enabled = true,  speed = 5.39, bezier = "easeOutQuint" })
hl.animation({ leaf = "windows",       enabled = true,  speed = 4.79, spring = "easy" })
hl.animation({ leaf = "windowsIn",     enabled = true,  speed = 4.1,  spring = "easy",         style = "popin 87%" })
hl.animation({ leaf = "windowsOut",    enabled = true,  speed = 1.49, bezier = "linear",       style = "popin 87%" })
hl.animation({ leaf = "fadeIn",        enabled = true,  speed = 1.73, bezier = "almostLinear" })
hl.animation({ leaf = "fadeOut",       enabled = true,  speed = 1.46, bezier = "almostLinear" })
hl.animation({ leaf = "fade",          enabled = true,  speed = 3.03, bezier = "quick" })
hl.animation({ leaf = "layers",        enabled = true,  speed = 3.81, bezier = "easeOutQuint" })
hl.animation({ leaf = "layersIn",      enabled = true,  speed = 4,    bezier = "easeOutQuint", style = "fade" })
hl.animation({ leaf = "layersOut",     enabled = true,  speed = 1.5,  bezier = "linear",       style = "fade" })
hl.animation({ leaf = "fadeLayersIn",  enabled = true,  speed = 1.79, bezier = "almostLinear" })
hl.animation({ leaf = "fadeLayersOut", enabled = true,  speed = 1.39, bezier = "almostLinear" })
hl.animation({ leaf = "workspaces",    enabled = true,  speed = 1.94, bezier = "almostLinear", style = "fade" })
hl.animation({ leaf = "workspacesIn",  enabled = true,  speed = 1.21, bezier = "almostLinear", style = "fade" })
hl.animation({ leaf = "workspacesOut", enabled = true,  speed = 1.94, bezier = "almostLinear", style = "fade" })
hl.animation({ leaf = "zoomFactor",    enabled = true,  speed = 7,    bezier = "quick" })

-- Ref https://wiki.hypr.land/Configuring/Basics/Workspace-Rules/
-- Pin workspaces to monitors so each display has its own set
hl.workspace_rule({ workspace = "1",  monitor = "DP-2",  default = true })
hl.workspace_rule({ workspace = "2",  monitor = "DP-2" })
hl.workspace_rule({ workspace = "3",  monitor = "DP-2" })
hl.workspace_rule({ workspace = "4",  monitor = "DP-2" })
hl.workspace_rule({ workspace = "5",  monitor = "DP-2" })
hl.workspace_rule({ workspace = "6",  monitor = "eDP-1", default = true })
hl.workspace_rule({ workspace = "7",  monitor = "eDP-1" })
hl.workspace_rule({ workspace = "8",  monitor = "eDP-1" })
hl.workspace_rule({ workspace = "9",  monitor = "eDP-1" })
hl.workspace_rule({ workspace = "10", monitor = "eDP-1" })

-- See https://wiki.hypr.land/Configuring/Layouts/Dwindle-Layout/ for more
hl.config({
    dwindle = {
        preserve_split = true, -- You probably want this
    },
})

-- See https://wiki.hypr.land/Configuring/Layouts/Master-Layout/ for more
hl.config({
    master = {
        new_status = "master",
    },
})

-- See https://wiki.hypr.land/Configuring/Layouts/Scrolling-Layout/ for more
hl.config({
    scrolling = {
        fullscreen_on_one_column = true,
    },
})

----------------
----  MISC  ----
----------------

hl.config({
    misc = {
        force_default_wallpaper  = 0,    -- Set to 0 or 1 to disable the anime mascot wallpapers
        disable_hyprland_logo    = true, -- If true disables the random hyprland logo / anime girl background. :(
        disable_splash_rendering = true, -- Disable the splash text on desktop
        vrr                      = 2,    -- Variable refresh rate: 0=off, 1=on, 2=fullscreen only, 3=fullscreen for games
        focus_on_activate        = true, -- Auto-focus windows that request activation (e.g. "open in browser")
        animate_manual_resizes   = true, -- Smoother animation when resizing windows by hand
        animate_mouse_windowdragging = false, -- Whether or not to animate window drags
        enable_swallow           = true, -- Hide terminal when it launches a GUI app (e.g. nautilus from kitty)
        swallow_regex            = "^(kitty|Alacritty|foot|com\\.mitchellh\\.ghostty|io\\.github\\.raphamorim\\.rio)$",
    },
})


----------------
---- CURSOR ----
----------------

hl.config({
    cursor = {
        no_hardware_cursors = true,  -- Fixes VRR flicker on AMD GPUs
        no_break_fs_vrr     = false, -- Allow VRR in fullscreen apps
    },
})


---------------
---- INPUT ----
---------------

hl.config({
    input = {
        kb_layout  = "us",
        kb_variant = "",
        kb_model   = "",
        kb_options = "caps:escape_shifted_capslock",
        kb_rules   = "",

        follow_mouse = 1,

        sensitivity = 0, -- -1.0 - 1.0, 0 means no modification.

        touchpad = {
            natural_scroll = true,
            disable_while_typing = true,
            clickfinger_behavior = true,
            scroll_factor = 1.0,
        },
    },
})

hl.gesture({
    fingers = 3,
    direction = "horizontal",
    action = "workspace"
})

-- Example per-device config
-- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Devices/ for more
hl.device({
    name        = "epic-mouse-v1",
    sensitivity = -0.5,
})


---------------------
---- KEYBINDINGS ----
---------------------

local mainMod = "SUPER" -- Sets "Windows" key as main modifier

-- Example binds, see https://wiki.hypr.land/Configuring/Basics/Binds/ for more
hl.bind(mainMod .. " + Return",      hl.dsp.exec_cmd(terminal))
local closeWindowBind = hl.bind(mainMod .. " + W", hl.dsp.window.close())
-- closeWindowBind:set_enabled(false)
hl.bind(mainMod .. " + F",           hl.dsp.window.fullscreen())
hl.bind(mainMod .. " + SHIFT + Q", hl.dsp.exit())
hl.bind(mainMod .. " + E",           hl.dsp.exec_cmd(fileManager))
hl.bind(mainMod .. " + V",           hl.dsp.window.float({ action = "toggle" }))

-- Toggle all windows on the current workspace between tiled and floating
local workspaceFloatState = {}
hl.bind(mainMod .. " + SHIFT + T", function()
    local ws = hl.get_active_workspace()
    if not ws then return end
    local makeFloat = not workspaceFloatState[ws.id]
    workspaceFloatState[ws.id] = makeFloat
    for _, w in ipairs(ws:get_windows()) do
        if w.floating ~= makeFloat then
            hl.dispatch(hl.dsp.window.float({ action = "toggle", window = "address:" .. w.address }))
        end
    end
end)
hl.bind(mainMod .. " + Space",       hl.dsp.exec_cmd(menu))
hl.bind(mainMod .. " + P",           hl.dsp.window.pseudo())
hl.bind(mainMod .. " + T",           hl.dsp.layout("togglesplit"))    -- dwindle only
hl.bind(mainMod .. " + CTRL + L",    hl.dsp.exec_cmd("hyprlock"))

-- Clipboard history (requires cliphist + fuzzel; daemon started in autostart)
hl.bind(mainMod .. " + SHIFT + V",   hl.dsp.exec_cmd("sh -c 'cliphist list | fuzzel --dmenu | cliphist decode | wl-copy'"))

-- Screenshots (hyprshot saves to ~/Pictures and copies to clipboard)
hl.bind(mainMod .. " + I",         hl.dsp.exec_cmd("hyprshot -m region"))
hl.bind(mainMod .. " + SHIFT + I", hl.dsp.exec_cmd("hyprshot -m output"))
hl.bind("Print",                   hl.dsp.exec_cmd("hyprshot -m region"))

-- Screen recording (wf-recorder; same bind toggles stop)
hl.bind(mainMod .. " + O",         hl.dsp.exec_cmd("screen-record region"))
hl.bind(mainMod .. " + SHIFT + O", hl.dsp.exec_cmd("screen-record output"))

-- Cycle workspaces with Tab
hl.bind(mainMod .. " + Tab",         hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mainMod .. " + SHIFT + Tab", hl.dsp.focus({ workspace = "e-1" }))

-- Resize submap: Super+R enters, hjkl resize, Escape/q exits
hl.bind(mainMod .. " + R", hl.dsp.submap("resize"))
hl.define_submap("resize", function()
    hl.bind("H",      hl.dsp.window.resize({ x = -40, y = 0,   relative = true }), { repeating = true })
    hl.bind("L",      hl.dsp.window.resize({ x = 40,  y = 0,   relative = true }), { repeating = true })
    hl.bind("K",      hl.dsp.window.resize({ x = 0,   y = -40, relative = true }), { repeating = true })
    hl.bind("J",      hl.dsp.window.resize({ x = 0,   y = 40,  relative = true }), { repeating = true })
    hl.bind("escape", hl.dsp.submap("reset"))
    hl.bind("Q",      hl.dsp.submap("reset"))
    hl.bind("Return", hl.dsp.submap("reset"))
end)

-- Move focus with mainMod + arrow keys
hl.bind(mainMod .. " + H",  hl.dsp.focus({ direction = "left" }))
hl.bind(mainMod .. " + L", hl.dsp.focus({ direction = "right" }))
hl.bind(mainMod .. " + K",    hl.dsp.focus({ direction = "up" }))
hl.bind(mainMod .. " + J",  hl.dsp.focus({ direction = "down" }))

-- Swap windows with mainMod + SHIFT + HJKL (preserves tile tree / split direction)
hl.bind(mainMod .. " + SHIFT + H", hl.dsp.window.swap({ direction = "l" }))
hl.bind(mainMod .. " + SHIFT + L", hl.dsp.window.swap({ direction = "r" }))
hl.bind(mainMod .. " + SHIFT + K", hl.dsp.window.swap({ direction = "u" }))
hl.bind(mainMod .. " + SHIFT + J", hl.dsp.window.swap({ direction = "d" }))

-- Switch workspaces with mainMod + [0-9]
-- Move active window to a workspace with mainMod + SHIFT + [0-9]
for i = 1, 10 do
    local key = i % 10 -- 10 maps to key 0
    hl.bind(mainMod .. " + " .. key,             hl.dsp.focus({ workspace = i}))
    hl.bind(mainMod .. " + SHIFT + " .. key,     hl.dsp.window.move({ workspace = i }))
end

-- Example special workspace (scratchpad)
hl.bind(mainMod .. " + S",         hl.dsp.workspace.toggle_special("magic"))
hl.bind(mainMod .. " + SHIFT + S", hl.dsp.window.move({ workspace = "special:magic" }))

-- Scroll through existing workspaces with mainMod + scroll
hl.bind(mainMod .. " + mouse_down", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mainMod .. " + mouse_up",   hl.dsp.focus({ workspace = "e-1" }))

-- Move/resize windows with mainMod + LMB/RMB and dragging
hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(),   { mouse = true })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

-- Laptop multimedia keys for volume and LCD brightness
hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"), { locked = true, repeating = true })
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%-"), { locked = true, repeating = true })
hl.bind("XF86AudioMute",        hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"),     { locked = true, repeating = true })
hl.bind("XF86AudioMicMute",     hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"),   { locked = true, repeating = true })
hl.bind("XF86MonBrightnessUp",  hl.dsp.exec_cmd("brightnessctl -e4 -n2 set 5%+"),                  { locked = true, repeating = true })
hl.bind("XF86MonBrightnessDown",hl.dsp.exec_cmd("brightnessctl -e4 -n2 set 5%-"),                  { locked = true, repeating = true })

-- Requires playerctl
hl.bind("XF86AudioNext",  hl.dsp.exec_cmd("playerctl next"),       { locked = true })
hl.bind("XF86AudioPause", hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPlay",  hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPrev",  hl.dsp.exec_cmd("playerctl previous"),   { locked = true })


--------------------------------
---- WINDOWS AND WORKSPACES ----
--------------------------------

-- See https://wiki.hypr.land/Configuring/Basics/Window-Rules/
-- and https://wiki.hypr.land/Configuring/Basics/Workspace-Rules/

-- Example window rules that are useful

local suppressMaximizeRule = hl.window_rule({
    -- Ignore maximize requests from all apps. You'll probably like this.
    name  = "suppress-maximize-events",
    match = { class = ".*" },

    suppress_event = "maximize",
})
-- suppressMaximizeRule:set_enabled(false)

hl.window_rule({
    -- Fix some dragging issues with XWayland
    name  = "fix-xwayland-drags",
    match = {
        class      = "^$",
        title      = "^$",
        xwayland   = true,
        float      = true,
        fullscreen = false,
        pin        = false,
    },

    no_focus = true,
})

-- Layer rules also return a handle.
-- local overlayLayerRule = hl.layer_rule({
--     name  = "no-anim-overlay",
--     match = { namespace = "^my-overlay$" },
--     no_anim = true,
-- })
-- overlayLayerRule:set_enabled(false)

-- Hyprland-run windowrule
hl.window_rule({
    name  = "move-hyprland-run",
    match = { class = "hyprland-run" },

    float  = true,
    center = true,
})

-- Float common file/save/open/auth dialogs in the center of the screen
hl.window_rule({
    name   = "float-dialogs",
    match  = { title = "^(Open|Save|Choose|Select|Authentication|Library) ?.*" },
    float  = true,
    center = true,
})

-- Picture-in-Picture: floating, pinned across workspaces, bottom-right
hl.window_rule({
    name  = "pip",
    match = { title = ".*[Pp]icture[- ]in[- ][Pp]icture.*" },
    float = true,
    pin   = true,
    size  = "480 270",
    move  = "100%-500 100%-290",
})

-- Inhibit screen idle/lock while a fullscreen video is playing
hl.window_rule({
    name         = "idle-inhibit-fullscreen",
    match        = { fullscreen = true },
    idle_inhibit = "focus",
})

-- Common always-floating utility apps
hl.window_rule({
    name   = "float-utilities",
    match  = { class = "^(pavucontrol|blueman-manager|nm-connection-editor|file-roller|qalculate-gtk)$" },
    float  = true,
    center = true,
})
