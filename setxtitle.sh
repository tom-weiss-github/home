#! /bin/sh
# This will change the xterm (or rxvt) title.
# The usage is to pass in the name in quotes after this script.
# For example, setxtitle.sh "new name"
# This should work in Cygwin and Red Hat.
echo -en "\033]0;$1\007"
#end
