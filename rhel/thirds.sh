#!/bin/bash

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
emacs='emacs-24_3.Emacs'
chrome='chromium-browser.Chromium-browser'

# Some useful commands when callibrating this script.
# wmctrl -lG will show x-offset, y-offset, width, and height.
# xprop -id 0x00001234 (0x00001234 from wmctrl will list properties of the window).

program_count $terminator
program_count $emacs
program_count $chrome

terminator_id=`wmctrl -lx | grep $terminator | tr -s " " | cut -d " " -f 1`
chrome_id=`wmctrl -lx | grep $chrome | tr -s " " | cut -d " " -f 1`
# The 'grep -v xps' is there because I don't want to use emacs from my xps host, but from another
# host.  Part of the output of the command contains the client host.
emacs_id=`wmctrl -lx | grep $emacs | grep -v xps | tr -s " " | cut -d " " -f 1`

echo terminator_id is $terminator_id
echo chrome_id is $chrome_id
echo emacs_id is $emacs_id

if [ "$1" == "off" ]; then
    wmctrl -r $terminator_id -i -b add,maximized_vert,maximized_horz
    wmctrl -r $emacs_id -i -b add,maximized_vert,maximized_horz
    wmctrl -r $chrome_id -i -b add,maximized_vert,maximized_horz
    # Give chrome focus after we are done.
    wmctrl -i -a $chrome_id
    exit 0
fi

if ! [ -z $terminator_id ]; then
    # If a window is maximized, then its dimensions cannot be changed.  This will remove
    # maximization if it's set.
    wmctrl -r $terminator_id -i -b remove,maximized_vert,maximized_horz
    wmctrl -r $terminator_id -i -e 0,0,0,1050,1387
    wmctrl -r $terminator_id -i -b add,maximized_vert
fi

if ! [ -z $emacs_id ]; then
    wmctrl -r $emacs_id -i -b remove,maximized_vert,maximized_horz
    wmctrl -r $emacs_id -i -e 0,1050,0,1190,1386
    wmctrl -r $emacs_id -i -b add,maximized_vert
fi

if ! [ -z $chrome_id ]; then
    wmctrl -r $chrome_id -i -b remove,maximized_vert,maximized_horz
    wmctrl -r $chrome_id -i -e 0,2240,0,1200,1367
    wmctrl -r $chrome_id -i -b add,maximized_vert
    # Give chrome focus after we are done.
    wmctrl -i -a $chrome_id
fi
