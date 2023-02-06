#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

#xset r rate 300 60 &
xmobar &
picom &
trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --height 16 --distancefrom top --distance 2 --iconspacing 2 --margin 2 --transparent true --alpha 0 --tint 0x202229 --widthtype request &
xsetroot -cursor_name left_ptr &
#nitrogen --restore &
