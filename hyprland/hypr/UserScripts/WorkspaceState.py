#!/usr/bin/env python3
# Feeds waybar custom/ws1..6 modules: listens to Hyprland socket2 events and
# writes one JSON per workspace into $XDG_RUNTIME_DIR/waybar-ws, then signals waybar.
import json
import os
import socket
import subprocess
import time

WORKSPACES = range(1, 7)
SIGNAL = "-RTMIN+8"
DOT = "•"
runtime = os.environ.get("XDG_RUNTIME_DIR", "/tmp")
out_dir = os.path.join(runtime, "waybar-ws")
os.makedirs(out_dir, exist_ok=True)

TRIGGERS = (b"workspace", b"openwindow", b"closewindow", b"movewindow",
            b"createworkspace", b"destroyworkspace", b"focusedmon", b"activespecial")


def hyprctl_json(*args):
    return json.loads(subprocess.run(["hyprctl", "-j", *args], capture_output=True, text=True).stdout or "null")


def update():
    workspaces = {w["id"]: w for w in hyprctl_json("workspaces") or []}
    active = (hyprctl_json("activeworkspace") or {}).get("id")
    for i in WORKSPACES:
        windows = workspaces.get(i, {}).get("windows", 0)
        cls = "active" if i == active else ("occupied" if windows else "empty")
        text = f"{i}{DOT}" if windows else f"{i} "
        tooltip = f"Bureau {i} : {windows} fenêtre(s)"
        tmp = os.path.join(out_dir, f"{i}.json.tmp")
        with open(tmp, "w") as f:
            json.dump({"text": text, "class": cls, "tooltip": tooltip}, f)
        os.replace(tmp, os.path.join(out_dir, f"{i}.json"))
    subprocess.run(["pkill", SIGNAL, "-x", "waybar"])


def main():
    sock_path = os.path.join(runtime, "hypr", os.environ["HYPRLAND_INSTANCE_SIGNATURE"], ".socket2.sock")
    while True:
        try:
            update()
            with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as s:
                s.connect(sock_path)
                buf = b""
                while True:
                    data = s.recv(4096)
                    if not data:
                        break
                    buf += data
                    *lines, buf = buf.split(b"\n")
                    if any(line.startswith(TRIGGERS) for line in lines):
                        update()
        except (OSError, ValueError):
            time.sleep(1)


if __name__ == "__main__":
    main()
