#!/usr/bin/env bash
# Module Waybar pour lowfi (lancé en arrière-plan, piloté via MPRIS/playerctl)
#   Lowfi.sh status  -> JSON pour waybar
#   Lowfi.sh toggle  -> démarre / arrête lowfi
#   Lowfi.sh menu    -> menu rofi (pause/lecture, suivant, arrêter)

LOWFI="$HOME/.cargo/bin/lowfi"
LOWFI_ARGS="-m --fps 1"
SIGNAL=9

refresh() { pkill -RTMIN+$SIGNAL waybar; }
running() { pgrep -x lowfi >/dev/null; }

start() {
	# lowfi est une TUI : on lui donne un pseudo-terminal invisible via `script`.
	# stdin reste ouvert (sleep infinity), sinon l'EOF met lowfi en pause.
	setsid bash -c "script -qfec '$LOWFI $LOWFI_ARGS' /dev/null < <(exec -a lowfi-stdin sleep infinity) >/dev/null 2>&1" &
	disown
	# Force la lecture une fois le lecteur MPRIS disponible
	for _ in $(seq 20); do
		playerctl -p lowfi status &>/dev/null && { playerctl -p lowfi play; break; }
		sleep 0.5
	done
}

stop() {
	pkill -x lowfi
	pkill -f '^lowfi-stdin' # ferme stdin : `script` peut alors se terminer
}

case "$1" in
status)
	if ! running; then
		echo '{"text": "󰽰 lowfi", "tooltip": "lowfi : arrêté (clic pour lancer)", "class": "stopped"}'
		exit 0
	fi
	status=$(playerctl -p lowfi status 2>/dev/null)
	title=$(playerctl -p lowfi metadata --format '{{markup_escape(title)}}' 2>/dev/null)
	artist=$(playerctl -p lowfi metadata --format '{{markup_escape(artist)}}' 2>/dev/null)
	case "$status" in
	Playing) icon="󰝚" class="playing" ;;
	Paused) icon="󰏤" class="paused" ;;
	*) icon="󰔟" class="loading" title="Chargement…" ;;
	esac
	tooltip="lowfi : ${title:-…}${artist:+ — $artist}\nClic : arrêter · Clic droit : menu"
	printf '{"text": "%s lowfi", "tooltip": "%s", "class": "%s"}\n' "$icon" "$tooltip" "$class"
	;;
toggle)
	if running; then stop; else start; fi
	sleep 0.5
	refresh
	;;
menu)
	running || exit 0
	if [ "$(playerctl -p lowfi status 2>/dev/null)" = "Playing" ]; then
		pp="󰏤  Pause"
	else
		pp="󰐊  Lecture"
	fi
	choice=$(printf '%s\n󰒭  Suivant\n󰓛  Arrêter\n' "$pp" | rofi -dmenu -i -p "lowfi")
	case "$choice" in
	*Pause | *Lecture) playerctl -p lowfi play-pause ;;
	*Suivant) playerctl -p lowfi next ;;
	*Arrêter) stop ;;
	esac
	sleep 0.3
	refresh
	;;
esac
