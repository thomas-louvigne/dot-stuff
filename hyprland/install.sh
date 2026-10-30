#!/usr/bin/env bash
# Install the Hyprland setup from this folder on a fresh Ubuntu 24.04 (noble).
# Usage: ./install.sh            (packages + configs)
#        ./install.sh --configs  (configs only, no apt/flatpak/fonts)
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
CONFIG_DIRS=(hypr waybar kitty rofi swaync wlogout wallust quickshell)
STAMP="$(date +%Y%m%d-%H%M)"

install_packages() {
    echo "==> PPA Hyprland (cppiber) + paquets apt"
    sudo add-apt-repository -y ppa:cppiber/hyprland
    sudo apt update
    sudo apt install -y \
        hyprland hypridle hyprlock xdg-desktop-portal-hyprland hyprland-guiutils \
        waybar kitty sway-notification-center wlogout thunar \
        network-manager-gnome nm-tray blueman \
        wl-clipboard cliphist grim slurp \
        pipewire-pulse wireplumber pulseaudio-utils pamixer pavucontrol playerctl brightnessctl \
        jq bc imagemagick libnotify-bin python3 rsync curl unzip \
        polkit-kde-agent-1 qt5ct qt6ct qt5-style-kvantum flatpak

    echo "==> Waterfox (Flatpak)"
    flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
    flatpak install -y flathub net.waterfox.waterfox

    echo "==> Polices Nerd Font (JetBrainsMono, FantasqueSansMono)"
    local fonts="$HOME/.local/share/fonts"
    mkdir -p "$fonts"
    for f in JetBrainsMono FantasqueSansMono; do
        if ! fc-list | grep -qi "${f%Mono}.*Nerd"; then
            curl -fsSL -o "/tmp/$f.zip" "https://github.com/ryanoasis/nerd-fonts/releases/latest/download/$f.zip"
            unzip -oq "/tmp/$f.zip" -d "$fonts/${f}Nerd"
        fi
    done
    fc-cache -f >/dev/null
}

install_configs() {
    echo "==> Copie des configs dans ~/.config (anciennes versions -> *.bak-$STAMP)"
    for d in "${CONFIG_DIRS[@]}"; do
        if [ -e "$HOME/.config/$d" ]; then
            mv "$HOME/.config/$d" "$HOME/.config/$d.bak-$STAMP"
        fi
        rsync -a "$HERE/$d/" "$HOME/.config/$d/"
    done

    # Absolute symlinks point to the original machine's home: recreate them
    ln -sfn "$HOME/.config/waybar/configs/[TOP] Default Laptop" "$HOME/.config/waybar/config"
    ln -sfn "$HOME/Pictures/wallpapers/Dynamic-Wallpapers/Dark/Tree_dark.png" "$HOME/.config/rofi/.current_wallpaper"

    chmod +x "$HOME"/.config/hypr/scripts/* "$HOME"/.config/hypr/UserScripts/* 2>/dev/null || true
    mkdir -p "$HOME/Pictures/wallpapers"
}

check_missing() {
    echo "==> Vérification des outils (à installer à la main si MANQUANT, voir README.org)"
    for b in Hyprland waybar kitty rofi swww swww-daemon wallust swaync swappy hypridle hyprlock \
             cliphist jq pactl wpctl playerctl brightnessctl thunar wlogout qs hyprgammad flatpak; do
        command -v "$b" >/dev/null && printf '  ok       %s\n' "$b" || printf '  MANQUANT %s\n' "$b"
    done
}

if [ "${1:-}" != "--configs" ]; then
    install_packages
fi
install_configs
check_missing
echo "==> Terminé. Déconnecte-toi puis choisis la session Hyprland."
