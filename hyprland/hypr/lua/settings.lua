-- General settings, decorations, animations, input
-- (from configs/SystemSettings.conf, UserConfigs/UserSettings.conf,
--  UserConfigs/UserDecorations.conf, UserConfigs/UserAnimations.conf, Laptops.conf)

local c = require("lua.colors")

hl.config({
    general = {
        border_size = 2,
        gaps_in     = 2,
        gaps_out    = 4,
        col = {
            active_border   = c.color12,
            inactive_border = c.color10,
        },
        resize_on_border = true,
        layout           = "dwindle",
    },

    decoration = {
        rounding           = 10,
        active_opacity     = 1.0,
        inactive_opacity   = 0.9,
        fullscreen_opacity = 1.0,
        dim_inactive       = true,
        dim_strength       = 0.1,
        dim_special        = 0.8,

        shadow = {
            enabled        = true,
            range          = 3,
            render_power   = 1,
            color          = c.color12,
            color_inactive = c.color10,
        },

        blur = {
            enabled           = true,
            size              = 6,
            passes            = 2,
            ignore_opacity    = true,
            new_optimizations = true,
            special           = true,
            popups            = true,
        },
    },

    group = {
        col      = { border_active = c.color15 },
        groupbar = { col = { active = c.color0 } },
    },

    animations = {
        enabled = true,
    },

    dwindle = {
        preserve_split       = true,
        special_scale_factor = 0.8,
    },

    master = {
        new_status = "master",
        new_on_top = true,
        mfact      = 0.5,
    },

    input = {
        kb_layout  = "us",
        kb_variant = "intl",
        kb_model   = "",
        kb_options = "ctrl:nocaps",
        kb_rules   = "",

        repeat_rate                = 50,
        repeat_delay               = 300,
        sensitivity                = 0, -- mouse sensitivity
        numlock_by_default         = true,
        left_handed                = false,
        follow_mouse               = 1,
        float_switch_override_focus = false,

        touchpad = {
            disable_while_typing    = true,
            natural_scroll          = false,
            clickfinger_behavior    = false,
            middle_button_emulation = false,
            tap_to_click            = true,
            drag_lock               = false,
        },

        touchdevice = { enabled = true },

        tablet = {
            transform   = 0,
            left_handed = false,
        },
    },

    gestures = {
        workspace_swipe_distance           = 500,
        workspace_swipe_invert             = true,
        workspace_swipe_min_speed_to_force = 30,
        workspace_swipe_cancel_ratio       = 0.5,
        workspace_swipe_create_new         = true,
        workspace_swipe_forever            = true,
    },

    misc = {
        disable_hyprland_logo      = true,
        disable_splash_rendering   = true,
        vrr                        = 2,
        mouse_move_enables_dpms    = true,
        enable_swallow             = false,
        swallow_regex              = "^(kitty)$",
        focus_on_activate          = false,
        initial_workspace_tracking = 0,
        middle_click_paste         = false,
        enable_anr_dialog          = true, -- Application not Responding (ANR)
        anr_missed_pings           = 15,   -- ANR Threshold default 1 is too low
        allow_session_lock_restore = true, -- Prevent lockscreen crash when resume from suspend
        on_focus_under_fullscreen  = 1,
    },

    binds = {
        workspace_back_and_forth = true,
        allow_workspace_cycles   = true,
        pass_mouse_when_bound    = false,
    },

    xwayland = {
        enabled            = true,
        force_zero_scaling = true,
    },

    render = {
        direct_scanout = 0,
    },

    cursor = {
        sync_gsettings_theme     = true,
        no_hardware_cursors      = 2, -- change to 1 if want to disable
        enable_hyprcursor        = true,
        warp_on_change_workspace = 2,
        no_warps                 = true,
    },
})

-- Laptop touchpad
hl.device({
    name    = "asue1209:00-04f3:319f-touchpad",
    enabled = true,
})

-- Animations
hl.curve("wind",      { type = "bezier", points = { {0.05, 0.9}, {0.1, 1.05} } })
hl.curve("winIn",     { type = "bezier", points = { {0.1, 1.1},  {0.1, 1.1}  } })
hl.curve("winOut",    { type = "bezier", points = { {0.3, -0.3}, {0, 1}      } })
hl.curve("liner",     { type = "bezier", points = { {1, 1},      {1, 1}      } })
hl.curve("overshot",  { type = "bezier", points = { {0.05, 0.9}, {0.1, 1.05} } })
hl.curve("smoothOut", { type = "bezier", points = { {0.5, 0},    {0.99, 0.99} } })
hl.curve("smoothIn",  { type = "bezier", points = { {0.5, -0.5}, {0.68, 1.5} } })

hl.animation({ leaf = "windows",       enabled = true, speed = 6,   bezier = "wind",      style = "slide" })
hl.animation({ leaf = "windowsIn",     enabled = true, speed = 5,   bezier = "winIn",     style = "slide" })
hl.animation({ leaf = "windowsOut",    enabled = true, speed = 3,   bezier = "smoothOut", style = "slide" })
hl.animation({ leaf = "windowsMove",   enabled = true, speed = 5,   bezier = "wind",      style = "slide" })
hl.animation({ leaf = "border",        enabled = true, speed = 1,   bezier = "liner" })
hl.animation({ leaf = "borderangle",   enabled = true, speed = 100, bezier = "liner",     style = "loop" }) -- was 180, max is now 100
hl.animation({ leaf = "fade",          enabled = true, speed = 3,   bezier = "smoothOut" })
hl.animation({ leaf = "workspaces",    enabled = true, speed = 5,   bezier = "overshot" })
hl.animation({ leaf = "workspacesIn",  enabled = true, speed = 5,   bezier = "winIn",     style = "slide" })
hl.animation({ leaf = "workspacesOut", enabled = true, speed = 5,   bezier = "winOut",    style = "slide" })
