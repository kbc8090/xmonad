#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

#xset r rate 300 60 &
trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --height 20 --iconspacing 3 --margin 3 --transparent true --alpha 0 --tint 0x202229 --widthtype request &
xmobar &
xsetroot -cursor_name left_ptr &
picom &
#nitrogen --restore &
