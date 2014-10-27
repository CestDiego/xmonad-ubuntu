#!/bin/bash
source $(dirname $0)/config.sh
XPOS=$((1180 + $XOFFSET))
WIDTH="180"
LINES="9"

playing=$(ncmpcpp --now-playing | awk ' { print $2}')
artist=$(ncmpcpp --now-playing | awk ' { print $2}')
album=$(ncmpcpp --now-playing | awk ' { print $4}')
track=$(ncmpcpp --now-playing | awk ' { print $4}')
date=$(mpc current -f  %date%)

art="/home/io/.config/ario/covers/$(ls ~/.config/ario/covers | "\
"  grep -v SMALL | grep "$(mpc current -f %album% | sed 's/:/ /g')")"
perc=`mpc | awk 'NR == 2 {gsub(/[()%]/,""); print $4}'`

percbar=`echo -e "$perc" | dzen2-gdbar -bg $bar_bg -fg $foreground -h 1 -w $(($WIDTH-20))`

#84x84
feh -x -B black -^ "" -g 108x108+$(($XPOS-108))+$(($YPOS+12)) -Z "$art" &
(echo "^fg($highlight)Music"; echo "       "; \
 echo "^ca(1,/home/io/.xmonad/scripts/dzen_lyrics.sh) " \
      "Track:  ^fg($highlight)$track^ca()"; \
 echo "^ca(1,/home/io/.xmonad/scripts/dzen_artistinfo.sh)^fg() "\
      "  Artist: ^fg($highlight)$artist^ca()"; \
 echo "^ca(1,/home/io/.xmonad/scripts/dzen_albuminfo.sh)^fg()  "
 "Album:  ^fg($highlight)$album^ca()"; \
 echo "^ca(1,/home/io/.xmonad/scripts/dzen_lyrics.sh)  "\
      "Year:   ^fg($highlight)$date^ca()"; 
 echo "  $percbar"; \
 echo "      ^ca(1, ncmpcpp prev)  "\
      "^fg($white0)^i(/home/io/.xmonad/dzen2/prev.xbm) "\
      "^ca()  ^ca(1, ncmpcpp pause) ^i(/home/io/.xmonad/dzen2/pause.xbm) "\
      "^ca()  ^ca(1, ncmpcpp play) ^i(/home/io/.xmonad/dzen2/play.xbm) "\
      "^ca()   ^ca(1, ncmpcpp next) ^i(/home/io/.xmonad/dzen2/next.xbm) ^ca()"; \
 echo " "; sleep 15) | \
    dzen2 \
        -fg $foreground \
        -bg $background \
        -fn $FONT \
        -x $XPOS -y $YPOS \
        -w $WIDTH -l $LINES \
        -e 'onstart=uncollapse,hide;button1=exit;button3=exit' & 
sleep 0.4
killall feh
