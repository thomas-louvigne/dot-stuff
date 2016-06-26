#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice

setxkbmap us -variant intl
# exec setxkbmap us -variant intl &

exec nm-applet --sm-disable &
exec xrdb -merge ~/.Xresources & # Truc pour xterm ? pas sur..
exec setxkbmap -option ctrl:nocaps & # Bind la touche capslock en controle
#exec thunar --deamon & #sert à rien , tjr aussi long à se lancer
exec dropbox &
exec owncloud &
exec emacs --daemon &


exec dbus-launch awesome
#exec dbus-launch startxfce4
