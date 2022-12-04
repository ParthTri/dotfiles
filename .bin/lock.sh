#!/bin/bash

### Pixelate Background
# rectangles=" "
#
# SR=$(xrandr --query | grep ' connected' | grep -o '[0-9][0-9]*x[0-9][0-9]*[^ ]*')
# for RES in $SR; do
#   SRA=(${RES//[x+]/ })
#   CX=$((${SRA[2]} + 25))
#   CY=$((${SRA[1]} - 80))
#   rectangles+="rectangle $CX,$CY $((CX+300)),$((CY-80)) "
# done
#
# TMPBG=/tmp/screen.png
# maim $TMPBG && convert $TMPBG -scale 5% -scale 2000% -draw "fill black fill-opacity 0.4 $rectangles" $TMPBG

# One Dark Colours
BLACK="282C34"
RED="E06C75"
GREEN="98C379"
YELLOW="E5C07B"
BLUE="61AFEF"
PURPLE="C678DD"
CYAN="56B6C2"
GREY="ABB2BF"

TRANSPARENT_GREY="4F5152C6"
TRANSPARENT="00000088"

i3lock \
	--blur=10 \
	--time-color=$CYAN --time-size=41  \
	--date-color=$BLUE --date-size=28 \
	--ring-color=$TRANSPARENT_GREY --inside-color=$TRANSPARENT_GREY\
	--clock \
	--radius=200 \
	--ringver-color=$GREEN --insidever-color=$GREEN \
	--ringwrong-color=$RED --insidewrong-color=$RED \
	--verif-text="Verifying" --wrong-text="Incorrect" \
	--keyhl-color=$GREEN --bshl-color=$RED \
	--noinput-text="No Input" \
	--indicator \
	# -i "$TMPBG" \

# rm $TMPBG
