#! /usr/bin/env python

# Author: Ulf Stegemann
# Converted from bash to python by Tom Weiss.

from optparse import OptionParser
import sys

usage = "\n%prog"
parser = OptionParser( usage=usage )

parser.add_option( "-v", "--verbose",
                   action="store_true", dest="verbose",
                   help="(Optional) Run in verbose mode." )

parser.add_option( "-n", "--no-execute",
                  action="store_true", dest="no_execute",
                  help="(Optional) Commands will not be executed.  "
                  "When used with -v, this allows users to see what "
                  "actions the script will perform without actually "
                  "executing them." )

( options, args ) = parser.parse_args()
  
# # test args
# if [ ! ${#} -ge 3 ]; then
#     echo 1>&2 "Usage: ${0} LOCAL REMOTE MERGED BASE"
#     echo 1>&2 "       (LOCAL, REMOTE, MERGED, BASE can be provided by \`git mergetool'.)"
#     exit 1
# fi
if( 3 > len(args) ):
    print( "Usage LOCAL REMOTE MERGED BASE" )
    sys.exit( 1 )


# # tools
# _EMACSCLIENT=/usr/bin/emacsclient
# _BASENAME=/bin/basename
# _CP=/bin/cp
# _EGREP=/bin/egrep
# _MKTEMP=/bin/mktemp

# # args
# _LOCAL=${1}
# _REMOTE=${2}
# _MERGED=${3}
local = args[0]
remote = args[1]
merged = args[2]
base = ""
ediff = ""
evaluate = ""

# if [ ${4} -a -r ${4} ] ; then
#     _BASE=${4}
#     _EDIFF=ediff-merge-files-with-ancestor
#     _EVAL="${_EDIFF} \"${_LOCAL}\" \"${_REMOTE}\" \"${_BASE}\" nil \"${_MERGED}\""
# elif [ ${_REMOTE} = ${_MERGED} ] ; then
#     _EDIFF=ediff
#     _EVAL="${_EDIFF} \"${_LOCAL}\" \"${_REMOTE}\""
# else
#     _EDIFF=ediff-merge-files
#     _EVAL="${_EDIFF} \"${_LOCAL}\" \"${_REMOTE}\" nil \"${_MERGED}\""
# fi
if( 4 == len(args) and
    os.path.exists(args[3]) ):
    base = args[3]
    ediff = "ediff-merge-files-with-ancestor"
    evalulate = "{0} \"{1}\" \"{2}\" \"{3}\" nil \"{4}\"".format( 
        ediff, local, remote, base, merged )
elif( remote == merged ):
    ediff = "ediff"
    evaluate = "{0} \"{1}\" \"{2}\"".format( ediff, local, remote )
else:
    ediff = "ediff-merge-files"
    evaluate = "{0} \"{1}\" \"{2}\" nil \"{3}\"".format( 
        ediff, local, remote, merged )
    

# # console vs. X
# if [ "${TERM}" = "linux" ]; then
#     unset DISPLAY
#     _EMACSCLIENTOPTS="-t"
# else
#     _EMACSCLIENTOPTS="-c"
# fi
emacs_client_options ="-c"

# # run emacsclient
# ${_EMACSCLIENT} ${_EMACSCLIENTOPTS} -a "" -e "(${_EVAL})" 2>&1
command = "emacsclient {0} -e ({1})".format( emacs_client_options, evaluate )
print( "{0}".format( command ) )

# # check modified file
# if [ ! $(egrep -c '^(<<<<<<<|=======|>>>>>>>|####### Ancestor)' ${_MERGED}) = 0 ]; then
#     _MERGEDSAVE=$(${_MKTEMP} --tmpdir `${_BASENAME} ${_MERGED}`.XXXXXXXXXX)
#     ${_CP} ${_MERGED} ${_MERGEDSAVE}
#     echo 1>&2 "Oops! Conflict markers detected in $_MERGED."
#     echo 1>&2 "Saved your changes to ${_MERGEDSAVE}"
#     echo 1>&2 "Exiting with code 1."
#     exit 1
# fi

# exit 0
sys.exit( 0 )
