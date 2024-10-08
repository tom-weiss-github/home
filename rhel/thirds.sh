#!/bin/bash

# This logic is referred to in ~/.local/share/applications/thirds.desktop
# [Desktop Entry]
# Version=1.0
# Name=thirds
# Exec=bash -i /home/tweiss/githome/rhel/thirds.sh on
# Terminal=false
# Icon=utilities-terminal
# Type=Application
#
# and in ~/.local/share/applications/nothirds.desktop
#
# [Desktop Entry]
# Version=1.0
# Name=nothirds
# Exec=bash -i /home/tweiss/githome/rhel/thirds.sh off
# Terminal=false
# Icon=utilities-terminal
# Type=Application

function program_count()
{
    # The -x shows the WM_CLASS and that seems to be unique for applications.
    application_count=`wmctrl -lx | grep -o $1 | wc -l`
    if [[ $application_count != "0" ]] && [[ $application_count != "1" ]]; then
        echo "Found $application_count instances of $1, there can be only 1 to resize."
        exit 1
    fi
}

logger -t thirds "Starting."


# These names are obtained from running 'wmctrl -lx'.
terminator='terminator.Terminator'
# emacs='emacs-24_3.Emacs' Worked for compiled CentOS 6.
emacs='emacs.Emacs'
# browser='chromium.Chromium'
# browser='Navigator.firefox'
browser='google-chrome.Google-chrome'


terminator_count=`wmctrl -lx | grep -o $terminator | wc -l`
emacs_count=`wmctrl -lx | grep -o $emacs | wc -l`
browser_count=`wmctrl -lx | grep -o $browser | wc -l`

echo terminator count is $terminator_count
echo emacs count is $emacs_count
echo browser count is $browser_count

# Some useful commands when callibrating this script.
# wmctrl -lG will show x-offset, y-offset, width, and height.
# xprop -id 0x00001234 (0x00001234 from wmctrl will list properties of the window).

terminator_id=`wmctrl -lx | grep $terminator | tr -s " " | cut -d " " -f 1`
browser_id=`wmctrl -lx | grep $browser | tr -s " " | cut -d " " -f 1`
# The 'grep -v xps' is there because I don't want to use emacs from my xps host, but from another
# host.  Part of the output of the command contains the client host.
emacs_id=`wmctrl -lx | grep $emacs | tr -s " " | cut -d " " -f 1`


echo terminator_id is $terminator_id
echo browser_id is $browser_id
echo emacs_id is $emacs_id

if [[ "$1" == "off" ]]; then
    echo off
    wmctrl -r $terminator_id -i -b add,maximized_vert,maximized_horz
    wmctrl -r $emacs_id -i -b add,maximized_vert,maximized_horz
    wmctrl -r $browser_id -i -b add,maximized_vert,maximized_horz
    # Give browser focus after we are done.
    wmctrl -i -a $browser_id
    exit 0
fi

# xrand4 | head -n 1
# Screen 0: minimum 320 x 200, current 3840 x 1600, maximum 16384 x 16384 # work screen
# Screen 0: minimum 320 x 200, current 1920 x 1200, maximum 16384 x 16384 # laptop screen
# Screen 0: minimum 320 x 200, current 3440 x 1440, maximum 16384 x 16384 # home

current_monitor=unset
current_width=`xrandr | head -n 1 | cut -d "," -f 2 | cut -d" " -f 3`
if [[ $current_width -eq 1920 ]]; then
    current_monitor="laptop"
elif [[ $current_width -eq 3440 ]]; then
    current_monitor="home"
elif [[ $current_width -eq 3840 ]]; then
    current_monitor="work"
fi
logger -t thirds "Monitor detected as ${current_monitor}."



if [[ ! -z $terminator_id && $terminator_count == 1 ]]; then
    echo Setting terminator.
    # If a window is maximized, then its dimensions cannot be changed.  This will remove
    # maximization if it's set.
    wmctrl -r $terminator_id -i -b remove,maximized_vert,maximized_horz
    if [[ $current_monitor == "home" ]]; then
        wmctrl -r $terminator_id -i -e 0,0,0,1050,1387
    elif [[ $current_monitor == "work" ]]; then
        wmctrl -r $terminator_id -i -e 0,0,0,1250,1387
    fi
    wmctrl -r $terminator_id -i -b add,maximized_vert
fi

if [[ ! -z $emacs_id && $emacs_count == 1 ]]; then
    echo Setting emacs.
    wmctrl -r $emacs_id -i -b remove,maximized_vert,maximized_horz
    if [[ $current_monitor == "home" ]]; then
        wmctrl -r $emacs_id -i -e 0,1050,0,1190,1386
    elif [[ $current_monitor == "work" ]]; then
        wmctrl -r $emacs_id -i -e 0,1250,0,1220,1386
        wmctrl -r $emacs_id -i -e 0,1250,0,1220,1386 # second one cause sometimes first needs help
    fi
    wmctrl -r $emacs_id -i -b add,maximized_vert
fi

if [[ ! -z $browser_id && $browser_count == 1 ]]; then
    echo Setting browser.
    wmctrl -r $browser_id -i -b remove,maximized_vert,maximized_horz
    if [[ $current_monitor == "home" ]]; then
        # don't think this one is needed wmctrl -r $browser_id -i -e 0,2240,0,1200,1367
        wmctrl -r $browser_id -i -e 0,2250,0,1250,1367
        wmctrl -r $browser_id -i -e 0,2250,0,1250,1367
        # 0:?  2250:position from left  0:?  1250:width  1367:?
    elif [[ $current_monitor == "work" ]]; then
        wmctrl -r $browser_id -i -e 0,2450,0,1420,1367
        sleep 0.3
        wmctrl -r $browser_id -i -e 0,2450,0,1420,1367
    fi
    wmctrl -r $browser_id -i -b add,maximized_vert
    # Give browser focus after we are done.
    wmctrl -i -a $browser_id
fi
