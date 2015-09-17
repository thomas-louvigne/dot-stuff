#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice

#exec nm-applet --sm-disable &
exec xrdb -merge ~/.Xresources & # Truc pour xterm ? pas sur..
exec setxkbmap -option ctrl:nocaps & # Bind la touche capslock en controle
#exec thunar --deamon & #sert à rien , tjr aussi long à se lancer
#exec dropbox &
exec emacs --daemon &


exec dbus-launch awesome
#exec dbus-launch startxfce4
