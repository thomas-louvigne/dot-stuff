-- Autostart (from configs/Startup_Apps.conf + UserConfigs/Startup_Apps.conf, deduplicated)

local home        = os.getenv("HOME")
local scriptsDir  = home .. "/.config/hypr/scripts"

local apps = {
    home .. "/.config/hypr/initial-boot.sh",
    "swww-daemon --format xrgb",
    "dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP",
    "systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP",
    scriptsDir .. "/Dropterminal.sh kitty",
    scriptsDir .. "/Polkit.sh",
    "nm-applet --indicator",
    "nm-tray", -- For ubuntu
    "swaync",
    "waybar",
    home .. "/.config/hypr/UserScripts/WorkspaceState.py",
    "qs -c overview", -- Quickshell Overview
    "hypridle",
    scriptsDir .. "/Hyprsunset.sh init",
    "wl-paste --type text --watch cliphist store",
    "wl-paste --type image --watch cliphist store",
    "blueman-applet",
    scriptsDir .. "/ChangeLayout.sh init",
    home .. "/.local/bin/hyprgammad " .. (os.getenv("XDG_RUNTIME_DIR") or "/run/user/1000") .. "/hyprgammad.fifo",
}

hl.on("hyprland.start", function()
    for _, cmd in ipairs(apps) do
        hl.exec_cmd(cmd)
    end
end)
