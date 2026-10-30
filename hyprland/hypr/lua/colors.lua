-- Reads the wallust palette ($colorN = rgb(XXXXXX)) from wallust-hyprland.conf,
-- so theme switching via wallust keeps working with the Lua config.

local M = {
    -- fallback palette, used if the wallust file is missing
    background = "rgb(010102)", foreground = "rgb(FDF8FE)",
    color0  = "rgb(313131)", color10 = "rgb(2D1D5F)",
    color12 = "rgb(7D4AB4)", color15 = "rgb(F3ECF5)",
}

local f = io.open(os.getenv("HOME") .. "/.config/hypr/wallust/wallust-hyprland.conf", "r")
if f then
    for line in f:lines() do
        local name, value = line:match("^%s*%$([%w_]+)%s*=%s*(%S+)")
        if name then M[name] = value end
    end
    f:close()
end

return M
