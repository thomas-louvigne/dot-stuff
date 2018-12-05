# 2 Screen
exec xrandr --output VIRTUAL1 --off --output eDP1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output DP1 --off --output HDMI2 --off --output HDMI1 --off --output DP2 --mode 1920x1080 --pos 1920x0 --rotate normal &


exec nm-applet --sm-disable > /dev/null &
exec setxkbmap -option ctrl:nocaps & # Bind la touche capslock en controle &
exec dropbox > /dev/null &
exec owncloud > /dev/null &
exec rambox 2> /dev/null &
exec firefox > /dev/null &
exec emacs --daemon > /dev/null &
# exec python2 /usr/bin/gcalcli agenda 7am 11:55pm &
