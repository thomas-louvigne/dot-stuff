#!/usr/bin/env bash
# Pick the default audio output (sink) or input (source) with rofi.
# Usage: AudioSwitch.sh [sink|source]   (default: sink)

kind="${1:-sink}"
case "$kind" in
sink)   list=sinks;   default=$(pactl get-default-sink);   prompt="Sortie son" ;;
source) list=sources; default=$(pactl get-default-source); prompt="Entrée son" ;;
*) echo "usage: $0 [sink|source]" >&2; exit 1 ;;
esac

# "name<TAB>description", skipping monitor sources
devices=$(pactl -f json list "$list" | jq -r '.[] | select(.name | test("\\.monitor$") | not) | "\(.name)\t\(.description)"')
[ -z "$devices" ] && { notify-send -u low "$prompt" "Aucun périphérique"; exit 1; }

menu=$(while IFS=$'\t' read -r name desc; do
    if [ "$name" = "$default" ]; then echo "● $desc"; else echo "   $desc"; fi
done <<<"$devices")

pkill rofi
choice=$(echo "$menu" | rofi -dmenu -i -p "$prompt" -no-custom)
[ -z "$choice" ] && exit 0
choice_desc=$(echo "$choice" | sed -E 's/^(● |   )//')
target=$(awk -F'\t' -v d="$choice_desc" '$2 == d { print $1; exit }' <<<"$devices")
[ -z "$target" ] && exit 1

pactl "set-default-$kind" "$target"

# Move streams already playing/recording to the new device
if [ "$kind" = sink ]; then
    for id in $(pactl list short sink-inputs | cut -f1); do pactl move-sink-input "$id" "$target"; done
else
    for id in $(pactl list short source-outputs | cut -f1); do pactl move-source-output "$id" "$target"; done
fi

notify-send -u low "$prompt" "$choice_desc"
