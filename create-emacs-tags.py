import string
import os
import sys
import subprocess
from optparse import OptionParser

ETAGS = "etags"
EBROWSE = 'ebrowse'


def AppendTags( tag_file_name, tag_files, verbose, no_execute ):
    cmd = "{0} --append --output={1} --language=c++ {2}".format( ETAGS, tag_file_name, tag_files )
    if( True == verbose ):
        print( "{0}".format( cmd ) )

    if( False == no_execute ):
        p = subprocess.Popen( cmd, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE )
        (stdoutdata, stderrdata) = p.communicate()

def AppendBrowse( browse_file_name, browse_files, verbose, no_execute ):
    append_option = ""
    # The browse file must exist to use the append flag.
    if( True == os.path.exists( browse_file_name ) ):
        append_option = "--append"

    cmd = "{0} {1} --output-file={2} {3}".format( EBROWSE, append_option, browse_file_name, browse_files )
    if( True == verbose ):
        print( "{0}".format( cmd ) )

    if( False == no_execute ):
        p = subprocess.Popen( cmd, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE )
        (stdoutdata, stderrdata) = p.communicate()


usage = "\n%prog dest"
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

parser.set_defaults( verbose=False )
parser.set_defaults( no_execute=False )

( options, args ) = parser.parse_args()

if( 1 != len(args) ):
    print( "You must pass the destination of the tags file." )
    sys.exit( 0 )

dest = args[0]

print( "Create GNU Emacs TAGS/BROWSE." )
sys.stdout.flush()

tag_file_name = dest + "/TAGS"
browse_file_name = dest + "/BROWSE"

filtered_files = ""
count = 0
for (root, dirs, files) in os.walk( dest ):
    for name in files:
        a_file = str( os.path.join(root, name) )

        if( a_file.endswith( "h" )
            # or a_file.endswith( "cpp" )
            # or a_file.endswith( "inl" )
             ):
            a_file = a_file.replace( "\\", "/" )
            filtered_files += a_file
            filtered_files += " "
            count += 1

        if( 30 == count ):
            AppendTags( tag_file_name, filtered_files, options.verbose, options.no_execute )
            AppendBrowse( browse_file_name, filtered_files, options.verbose, options.no_execute )

            count = 0
            filtered_files = ""


AppendTags( tag_file_name, filtered_files, options.verbose, options.no_execute )
AppendBrowse( browse_file_name, filtered_files, options.verbose, options.no_execute )

print( "Created tags file, {0}.".format( tag_file_name ) )
print( "Created browse file, {0}.".format( browse_file_name ) )
print( "Finished creating GNU Emacs TAGS and BROWSE." )
