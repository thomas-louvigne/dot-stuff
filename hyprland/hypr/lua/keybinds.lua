-- Keybinds (ported from JaKooLit configs/Keybinds.conf)

local home        = os.getenv("HOME")
local scriptsDir  = home .. "/.config/hypr/scripts"
local UserScripts = home .. "/.config/hypr/UserScripts"
local term        = "kitty"
local files       = "thunar"
local mainMod     = "SUPER"

local function m(keys) return mainMod .. " + " .. keys end
local function exec(cmd) return hl.dsp.exec_cmd(cmd) end
local function bind(keys, dsp, desc, opts)
    opts = opts or {}
    opts.description = desc
    hl.bind(keys, dsp, opts)
end

local function zoom(factor)
    return function()
        local z = hl.get_config("cursor.zoom_factor") or 1
        if z < 1 then z = 1 end
        z = z * factor
        if z < 1 then z = 1 end
        hl.config({ cursor = { zoom_factor = z } })
    end
end

local function float_all()
    local ws = hl.get_active_workspace()
    if not ws then return end
    for _, w in ipairs(hl.get_workspace_windows(ws)) do
        hl.dispatch(hl.dsp.window.float({ window = w }))
    end
end

---- STANDARD ----
bind(m("D"),      exec("pkill rofi || true && rofi -show drun -modi drun,filebrowser,run,window"), "app launcher")
bind(m("B"),      exec("flatpak run net.waterfox.waterfox"), "open Waterfox")
bind(m("A"),      exec(scriptsDir .. "/OverviewToggle.sh"), "desktop overview")
bind(m("Return"), exec(term), "open terminal")
bind(m("E"),      exec(files), "file manager")

---- FEATURES / EXTRAS ----
bind(m("T"),                exec(scriptsDir .. "/ThemeChanger.sh"), "global theme switcher")
bind(m("H"),                exec(scriptsDir .. "/KeyHints.sh"), "help / cheat sheet")
bind(m("ALT + R"),          exec(scriptsDir .. "/Refresh.sh"), "refresh bar and menus")
bind(m("ALT + E"),          exec(scriptsDir .. "/RofiEmoji.sh"), "emoji menu")
bind(m("S"),                exec(scriptsDir .. "/RofiSearch.sh"), "web search")
bind(m("CTRL + S"),         exec("rofi -show window"), "window switcher")
bind(m("ALT + O"),          exec(scriptsDir .. "/ChangeBlur.sh"), "toggle blur")
bind(m("SHIFT + G"),        exec(scriptsDir .. "/GameMode.sh"), "toggle game mode")
bind(m("ALT + L"),          exec(scriptsDir .. "/ChangeLayout.sh"), "toggle master/dwindle layout")
bind(m("ALT + V"),          exec(scriptsDir .. "/ClipManager.sh"), "clipboard manager")
bind(m("CTRL + R"),         exec(scriptsDir .. "/RofiThemeSelector.sh"), "rofi theme selector")
bind(m("CTRL + SHIFT + R"), exec("pkill rofi || true && " .. scriptsDir .. "/RofiThemeSelector-modified.sh"), "rofi theme selector (modified)")

bind(m("SHIFT + F"),     hl.dsp.window.fullscreen({ mode = "fullscreen" }), "fullscreen")
bind(m("CTRL + F"),      hl.dsp.window.fullscreen({ mode = "maximized" }), "maximize window")
bind(m("SPACE"),         hl.dsp.window.float({ action = "toggle" }), "float current window")
bind(m("ALT + SPACE"),   float_all, "float all windows")
bind(m("SHIFT + Return"), exec(scriptsDir .. "/Dropterminal.sh " .. term), "dropdown terminal")

-- Desktop zooming or magnifier
bind(m("ALT + mouse_down"), zoom(2.0), "zoom in")
bind(m("ALT + mouse_up"),   zoom(0.5), "zoom out")

-- Waybar
bind(m("CTRL + ALT + B"), exec("pkill -SIGUSR1 waybar"), "toggle waybar on/off")
bind(m("CTRL + B"),       exec(scriptsDir .. "/WaybarStyles.sh"), "waybar styles menu")
bind(m("ALT + B"),        exec(scriptsDir .. "/WaybarLayout.sh"), "waybar layout menu")

bind(m("N"), exec(scriptsDir .. "/Hyprsunset.sh toggle"), "toggle night light")

-- UserScripts
bind(m("SHIFT + M"),  exec(UserScripts .. "/RofiBeats.sh"), "online music")
bind(m("W"),          exec(UserScripts .. "/WallpaperSelect.sh"), "select wallpaper")
bind(m("SHIFT + W"),  exec(UserScripts .. "/WallpaperEffects.sh"), "wallpaper effects")
bind("CTRL + ALT + W", exec(UserScripts .. "/WallpaperRandom.sh"), "random wallpaper")
bind(m("CTRL + O"),   hl.dsp.window.set_prop({ prop = "opaque", value = "toggle" }), "toggle active window opacity")
bind(m("SHIFT + K"),  exec(scriptsDir .. "/KeyBinds.sh"), "search keybinds")
bind(m("SHIFT + A"),  exec(scriptsDir .. "/Animations.sh"), "animations menu")
bind(m("SHIFT + O"),  exec(UserScripts .. "/ZshChangeTheme.sh"), "change oh-my-zsh theme")
bind(m("ALT + C"),    exec(UserScripts .. "/RofiCalc.sh"), "calculator")

-- Keyboard layout
bind("ALT + Shift_L", exec(scriptsDir .. "/KeyboardLayout.sh switch"), "switch keyboard layout globally", { locked = true, non_consuming = true })
bind("SHIFT + Alt_L", exec(scriptsDir .. "/Tak0-Per-Window-Switch.sh"), "switch keyboard layout per-window", { locked = true, non_consuming = true })

-- Move current workspace to monitors
bind(m("CTRL + F9"),  hl.dsp.workspace.move({ monitor = "l" }), "move workspace to left monitor")
bind(m("CTRL + F10"), hl.dsp.workspace.move({ monitor = "r" }), "move workspace to right monitor")
bind(m("CTRL + F11"), hl.dsp.workspace.move({ monitor = "u" }), "move workspace to up monitor")
bind(m("CTRL + F12"), hl.dsp.workspace.move({ monitor = "d" }), "move workspace to down monitor")

---- SYSTEM ----
bind("CTRL + ALT + Delete", hl.dsp.exit(), "exit Hyprland")
bind(m("Q"),                hl.dsp.window.close(), "close active window")
bind(m("SHIFT + Q"),        exec(scriptsDir .. "/KillActiveProcess.sh"), "terminate active process")
bind("CTRL + ALT + L",      exec(scriptsDir .. "/LockScreen.sh"), "lock screen")
bind("CTRL + ALT + P",      exec(scriptsDir .. "/Wlogout.sh"), "powermenu")
bind(m("SHIFT + N"),        exec("swaync-client -t -sw"), "notification panel")
bind(m("SHIFT + E"),        exec(scriptsDir .. "/Kool_Quick_Settings.sh"), "quick settings menu")

-- Master layout
bind(m("CTRL + D"),      hl.dsp.layout("removemaster"), "remove master")
bind(m("I"),             hl.dsp.layout("addmaster"), "add master")
bind(m("CTRL + Return"), hl.dsp.layout("swapwithmaster"), "swap with master")
bind(m("J"),             hl.dsp.window.cycle_next(), "cycle next")
bind(m("K"),             hl.dsp.window.cycle_next({ next = false }), "cycle previous")

-- Dwindle layout
bind(m("SHIFT + I"), hl.dsp.layout("togglesplit"), "toggle split (dwindle)")
bind(m("P"),         hl.dsp.window.pseudo(), "toggle pseudo (dwindle)")
bind(m("M"),         hl.dsp.layout("splitratio exact 0.3"), "set split ratio 0.3")

-- Cycle windows; if floating bring to top
bind("ALT + Tab", function()
    hl.dispatch(hl.dsp.window.cycle_next())
    hl.dispatch(hl.dsp.window.alter_zorder({ mode = "top" }))
end, "cycle next window")

---- SPECIAL KEYS ----
local el = { repeating = true, locked = true }
local l  = { locked = true }
bind("XF86AudioRaiseVolume",       exec(scriptsDir .. "/Volume.sh --inc"), "volume up", el)
bind("XF86AudioLowerVolume",       exec(scriptsDir .. "/Volume.sh --dec"), "volume down", el)
bind("ALT + XF86AudioRaiseVolume", exec(scriptsDir .. "/Volume.sh --inc-precise"), "volume up precise", el)
bind("ALT + XF86AudioLowerVolume", exec(scriptsDir .. "/Volume.sh --dec-precise"), "volume down precise", el)
bind("XF86AudioMicMute",   exec(scriptsDir .. "/Volume.sh --toggle-mic"), "toggle mic mute", l)
bind("XF86AudioMute",      exec(scriptsDir .. "/Volume.sh --toggle"), "toggle mute", l)
bind("XF86Sleep",          exec("systemctl suspend"), "sleep", l)
bind("XF86Rfkill",         exec(scriptsDir .. "/AirplaneMode.sh"), "airplane mode", l)
bind("XF86AudioPause",     exec(scriptsDir .. "/MediaCtrl.sh --pause"), "pause", l)
bind("XF86AudioPlay",      exec(scriptsDir .. "/MediaCtrl.sh --pause"), "play", l)
bind("XF86AudioNext",      exec(scriptsDir .. "/MediaCtrl.sh --nxt"), "next track", l)
bind("XF86AudioPrev",      exec(scriptsDir .. "/MediaCtrl.sh --prv"), "previous track", l)
bind("XF86AudioStop",      exec(scriptsDir .. "/MediaCtrl.sh --stop"), "stop", l)
bind("XF86MonBrightnessUp",   exec("brightnessctl -e4 -n2 set 5%+"), "brightness up", el)
bind("XF86MonBrightnessDown", exec("brightnessctl -e4 -n2 set 5%-"), "brightness down", el)

-- Screenshots
bind(m("Print"),              exec(scriptsDir .. "/ScreenShot.sh --now"), "screenshot now")
bind(m("SHIFT + Print"),      exec(scriptsDir .. "/ScreenShot.sh --area"), "screenshot (area)")
bind(m("CTRL + Print"),       exec(scriptsDir .. "/ScreenShot.sh --in5"), "screenshot in 5s")
bind(m("CTRL + SHIFT + Print"), exec(scriptsDir .. "/ScreenShot.sh --in10"), "screenshot in 10s")
bind("ALT + Print",           exec(scriptsDir .. "/ScreenShot.sh --active"), "screenshot active window")
bind(m("SHIFT + S"),          exec(scriptsDir .. "/ScreenShot.sh --swappy"), "screenshot (swappy)")

---- WINDOWS ----
local dirs = { left = { -50, 0 }, right = { 50, 0 }, up = { 0, -50 }, down = { 0, 50 } }
for dir, d in pairs(dirs) do
    bind(m("SHIFT + " .. dir), hl.dsp.window.resize({ x = d[1], y = d[2], relative = true }), "resize " .. dir, { repeating = true })
    bind(m("CTRL + " .. dir),  hl.dsp.window.move({ direction = dir }), "move window " .. dir)
    bind(m("ALT + " .. dir),   hl.dsp.window.swap({ direction = dir }), "swap window " .. dir)
    bind(m(dir),               hl.dsp.focus({ direction = dir }), "focus " .. dir)
end

-- Groups
bind(m("G"),            hl.dsp.group.toggle(), "toggle group")
bind(m("CTRL + Tab"),   hl.dsp.group.next(), "change active in group")
bind(m("CTRL + K"),     hl.dsp.window.move({ into_group = "left" }), "move left into group")
bind(m("CTRL + L"),     hl.dsp.window.move({ into_group = "right" }), "move right into group")
bind(m("CTRL + H"),     hl.dsp.window.move({ out_of_group = true }), "move active out of group")

---- WORKSPACES ----
bind(m("Tab"),         hl.dsp.focus({ workspace = "m+1" }), "next workspace")
bind(m("SHIFT + Tab"), hl.dsp.focus({ workspace = "m-1" }), "previous workspace")
bind(m("SHIFT + U"),   hl.dsp.window.move({ workspace = "special:magic" }), "move to special workspace")
bind(m("U"),           hl.dsp.workspace.toggle_special("magic"), "toggle special workspace")

for i = 1, 10 do
    local key = "code:" .. (9 + i) -- code:10 = key 1 ... code:19 = key 0
    bind(m(key),            hl.dsp.focus({ workspace = i }), "workspace " .. i)
    bind(m("SHIFT + " .. key), hl.dsp.window.move({ workspace = i }), "move to workspace " .. i)
    bind(m("CTRL + " .. key),  hl.dsp.window.move({ workspace = i, follow = false }), "move silently to workspace " .. i)
end

bind(m("SHIFT + bracketleft"),  hl.dsp.window.move({ workspace = "-1" }), "move to previous workspace")
bind(m("SHIFT + bracketright"), hl.dsp.window.move({ workspace = "+1" }), "move to next workspace")
bind(m("CTRL + bracketleft"),   hl.dsp.window.move({ workspace = "-1", follow = false }), "move silently to previous workspace")
bind(m("CTRL + bracketright"),  hl.dsp.window.move({ workspace = "+1", follow = false }), "move silently to next workspace")

bind(m("mouse_down"), hl.dsp.focus({ workspace = "e+1" }), "next workspace")
bind(m("mouse_up"),   hl.dsp.focus({ workspace = "e-1" }), "previous workspace")
bind(m("period"),     hl.dsp.focus({ workspace = "e+1" }), "next workspace")
bind(m("comma"),      hl.dsp.focus({ workspace = "e-1" }), "previous workspace")

-- Move/resize windows with mainMod + LMB/RMB and dragging
bind(m("mouse:272"), hl.dsp.window.drag(),   "move window",   { mouse = true })
bind(m("mouse:273"), hl.dsp.window.resize(), "resize window", { mouse = true })
