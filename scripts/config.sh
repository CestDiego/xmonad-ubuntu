#!/bin/bash

background="#1f1f1f"
foreground="#6d715e"
highlight="#c0b18b"

#XPOS=$(xdotool getmouselocation | awk -F " " '{print $1}' | cut -d ":" -f 2)
YPOS="11"
HEIGHT="20"
XOFFSET=300
if [[ -z `xrandr | grep " connected" | grep 'VGA'` ]]; then
	XOFFSET="0"
fi

#FONT="-artwiz-lime-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
FONT="xft:Monaco"
#FONT="-artwiz-limey-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
#FONT="-*-tamsyn-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
#FONT='-*-lemon-*-*-*-*-*-*-75-75-*-*-*-*'
#FONT="-*-tewi-medium-*-normal-*-*-*-*-*-*-*-*-*"
white0="#775759"
bar_bg="#3f3f3f"
bar_fg="#d17b49"
notify="#Cd3700"
warning="#8b0000"
