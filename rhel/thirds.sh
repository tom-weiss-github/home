#!/bin/bash

# These names are obtained from running 'wmctrl -lx'.
chrome='google-chrome.Google-chrome'
terminator='terminator.Terminator'

# The -x shows the WM_CLASS and that seems to be unique for chrome.  Confirmed if there are two
# chome instances then I get two values.  wmctrl -lx
chrome_count=`wmctrl -lx | grep -o $chrome | wc -l`
if [ $chrome_count != "1" ]; then
    echo "Found $chrome_count instances of Chrome, there can be only 1 to resize."
    exit 1
fi

# Next: Let's make the error checking into a function and update it so that it's okay with either 0
# or 1 instances.  Then invoke it for Terminator as well.


chrome_id=`wmctrl -lx | grep $chrome | tr -s " " | cut -d " " -f 1`
echo $chrome_id

# If a window is maximized, then its dimensions cannot be changed.
wmctrl -r $chrome_id -i -b remove,maximized_vert,maximized_horz

# Note there seems to be a bug in unity that leaves some space in the upper left when x,y are 0,0.
wmctrl -r $chrome_id -i -e 0,0,0,600,600
