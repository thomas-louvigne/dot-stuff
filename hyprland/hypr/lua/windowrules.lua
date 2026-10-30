-- Window and layer rules (from configs/WindowRules.conf)

local function rule(spec) hl.window_rule(spec) end

---------------
---- TAGS -----
---------------

local tags = {
    browser = {
        "^([Ff]irefox|org.mozilla.firefox|[Ff]irefox-esr|[Ff]irefox-bin)$",
        "^([Gg]oogle-chrome(-beta|-dev|-unstable)?)$",
        "^(chrome-.+-Default)$",
        "^([Cc]hromium)$",
        "^([Mm]icrosoft-edge(-stable|-beta|-dev|-unstable))$",
        "^(Brave-browser(-beta|-dev|-unstable)?)$",
        "^([Tt]horium-browser|[Cc]achy-browser)$",
        "^(zen-alpha|zen)$",
    },
    notif = { "^(swaync-control-center|swaync-notification-window|swaync-client|class)$" },
    ["KooL-Settings"] = { "^(nwg-displays|nwg-look)$" },
    terminal = { "^(Alacritty|kitty|kitty-dropterm)$" },
    email = {
        "^([Tt]hunderbird|org.mozilla.Thunderbird)$",
        "^(eu.betterbird.Betterbird)$",
        "^(org.gnome.Evolution)$",
    },
    projects = {
        "^(codium|codium-url-handler|VSCodium)$",
        "^(VSCode|code|code-url-handler)$",
        "^(jetbrains-.+)$",
        "^(dev.zed.Zed|antigravity)$",
    },
    screenshare = { "^(com.obsproject.Studio)$" },
    im = {
        "^([Dd]iscord|[Ww]ebCord|[Vv]esktop)$",
        "^([Ff]erdium)$",
        "^([Ww]hatsapp-for-linux)$",
        "^(org.telegram.desktop|io.github.tdesktop_x64.TDesktop)$",
        "^(teams-for-linux)$",
        "^(im.riot.Riot|Element)$",
    },
    games = { "^(gamescope)$", "^(steam_app_\\d+)$" },
    gamestore = { "^([Ss]team)$", "^(com.heroicgameslauncher.hgl)$" },
    ["file-manager"] = { "^([Tt]hunar|org.gnome.Nautilus|[Pp]cmanfm-qt)$", "^(app.drey.Warp)$" },
    wallpaper = { "^([Ww]aytrogen)$" },
    multimedia = { "^([Aa]udacious)$" },
    multimedia_video = { "^([Mm]pv|vlc)$" },
    settings = {
        "^(wihotspot(-gui)?)$",
        "^([Bb]aobab|org.gnome.[Bb]aobab)$",
        "^(gnome-disks|wihotspot(-gui)?)$",
        "^(file-roller|org.gnome.FileRoller)$",
        "^(nm-applet|nm-connection-editor|blueman-manager)$",
        "^(pavucontrol|org.pulseaudio.pavucontrol|com.saivert.pwvucontrol)$",
        "^(qt5ct|qt6ct)$",
        "(xdg-desktop-portal-gtk)",
        "^(org.kde.polkit-kde-authentication-agent-1)$",
        "^([Rr]ofi)$",
        "^(btrfs-assistant)$",
        "^(timeshift-gtk)$",
    },
    viewer = {
        "^(gnome-system-monitor|org.gnome.SystemMonitor|io.missioncenter.MissionCenter)$",
        "^(evince)$",
        "^(eog|org.gnome.Loupe)$",
    },
}

for tag, classes in pairs(tags) do
    for _, class in ipairs(classes) do
        rule({ match = { class = class }, tag = "+" .. tag })
    end
end

-- tags matched by title
rule({ match = { title = "^(KooL Quick Cheat Sheet)$" }, tag = "+KooL_Cheat" })
rule({ match = { title = "^(KooL Hyprland Settings)$" }, tag = "+KooL_Settings" })
rule({ match = { title = "^([Ll]utris)$" },              tag = "+gamestore" })
rule({ match = { title = "^(ROG Control)$" },            tag = "+settings" })
rule({ match = { title = "(Kvantum Manager)" },          tag = "+settings" })

-----------------
---- EFFECTS ----
-----------------

rule({ match = { tag = "multimedia_video" }, no_blur = true, opacity = "1.0" })
rule({ match = { tag = "multimedia" },       no_blur = true, opacity = "1.0" })

-- center
rule({ match = { tag = "KooL_Cheat" },            center = true })
rule({ match = { tag = "KooL-Settings" },         center = true })
rule({ match = { title = "^(ROG Control)$" },     center = true })
rule({ match = { title = "^(Keybindings)$" },     center = true })
rule({ match = { class = "^(pavucontrol|org.pulseaudio.pavucontrol|com.saivert.pwvucontrol)$" }, center = true })
rule({ match = { class = "^([Ff]erdium)$" },      center = true })

-- idle inhibit
rule({ match = { fullscreen = true }, idle_inhibit = "fullscreen" })
rule({ match = { class = ".*" },      idle_inhibit = "fullscreen" })

-- float
rule({ match = { tag = "KooL_Cheat" },    float = true })
rule({ match = { tag = "wallpaper" },     float = true, center = true })
rule({ match = { tag = "settings" },      float = true, center = true })
rule({ match = { tag = "viewer" },        float = true, center = true })
rule({ match = { tag = "KooL-Settings" }, float = true, center = true })
rule({ match = { class = "([Zz]oom|onedriver|onedriver-launcher)" },   float = true })
rule({ match = { class = "(org.gnome.Calculator|qalculate-gtk)" },     float = true })
rule({ match = { class = "^(mpv|com.github.rafostar.Clapper)$" },      float = true })
rule({ match = { class = "^([Qq]alculate-gtk)$" },                     float = true })
rule({ match = { class = "^([Ff]erdium)$" },                           float = true })
rule({ match = { title = "^(Authentication Required)$" },              float = true, center = true })
rule({ match = { class = "(codium|codium-url-handler|VSCodium)", title = "negative:(.*codium.*|.*VSCodium.*)" }, float = true })
rule({ match = { class = "^(com.heroicgameslauncher.hgl)$", title = "negative:(Heroic Games Launcher)" },       float = true })
rule({ match = { class = "^([Ss]team)$", title = "negative:^([Ss]team)$" },                                    float = true })
rule({ match = { title = "^(Add Folder to Workspace)$" }, float = true, size = "(monitor_w*0.7) (monitor_h*0.6)", center = true })
rule({ match = { title = "^(Save As)$" },                 float = true, size = "(monitor_w*0.7) (monitor_h*0.6)", center = true })
rule({ match = { initial_title = "(Open Files)" },        float = true, size = "(monitor_w*0.7) (monitor_h*0.6)" })
rule({ match = { title = "^(SDDM Background)$" },         float = true, center = true, size = "(monitor_w*0.16) (monitor_h*0.12)" })
rule({ match = { class = "^(yad)$" },                     float = true, center = true, size = "(monitor_w*0.2) (monitor_h*0.2)" })
rule({ match = { class = "^(hyprland-donate-screen)$" },  float = true, center = true })

-- opacity
rule({ match = { tag = "browser" },      opacity = "0.99 0.8" })
rule({ match = { tag = "projects" },     opacity = "0.9 0.8" })
rule({ match = { tag = "im" },           opacity = "0.94 0.86" })
rule({ match = { tag = "multimedia" },   opacity = "0.94 0.86" })
rule({ match = { tag = "file-manager" }, opacity = "0.9 0.8" })
rule({ match = { tag = "terminal" },     opacity = "0.9 0.7" })
rule({ match = { tag = "settings" },     opacity = "0.8 0.7" })
rule({ match = { tag = "viewer" },       opacity = "0.82 0.75" })
rule({ match = { tag = "wallpaper" },    opacity = "0.9 0.7" })
rule({ match = { class = "^(gedit|org.gnome.TextEditor|mousepad)$" }, opacity = "0.8 0.7" })
rule({ match = { class = "^(deluge)$" },                              opacity = "0.9 0.8" })
rule({ match = { class = "^(seahorse)$" },                            opacity = "0.9 0.8" })

-- size
rule({ match = { tag = "KooL_Cheat" },         size = "(monitor_w*0.65) (monitor_h*0.9)" })
rule({ match = { tag = "wallpaper" },          size = "(monitor_w*0.7) (monitor_h*0.7)" })
rule({ match = { tag = "settings" },           size = "(monitor_w*0.7) (monitor_h*0.7)" })
rule({ match = { class = "^([Ff]erdium)$" },   size = "(monitor_w*0.6) (monitor_h*0.7)" })

-- games
rule({ match = { tag = "games" }, no_blur = true, fullscreen = false })

-- no initial focus
rule({ match = { class = "^(jetbrains-*)" }, no_initial_focus = true })
rule({ match = { title = "^(wind.*)$" },     no_initial_focus = true })

-- SUPER CTRL + O toggles this tag (replaces "setprop active opaque toggle")
rule({ match = { tag = "opaque_toggle" }, opaque = true })

rule({
    name  = "Whatsapp-zapzap",
    match = { class = "^([Ww]hatsapp-for-linux|ZapZap|com.rtosta.zapzap)$" },
    size   = "(monitor_w*0.6) (monitor_h*0.7)",
    center = true,
})

rule({
    name  = "Picture-in-Picture",
    match = { title = "^(Picture-in-Picture)$" },
    float             = true,
    move              = "(monitor_w*0.72) (monitor_h*0.07)",
    opacity           = "0.95 0.75",
    pin               = true,
    keep_aspect_ratio = true,
    size              = "(monitor_w*0.3) (monitor_h*0.3)",
})

rule({
    name  = "Thunar-Progress-bar",
    match = { class = "^(thunar)$", title = "^(File Operation Progress)$" },
    float  = true,
    center = true,
    size   = "(monitor_w*0.26) (monitor_h*0.18)",
})

---------------------
---- LAYER RULES ----
---------------------

hl.layer_rule({ match = { namespace = "rofi" },                blur = true })
hl.layer_rule({ match = { namespace = "notifications" },       blur = true })
hl.layer_rule({ match = { namespace = "quickshell:overview" }, blur = true, ignore_alpha = 0.5 })
