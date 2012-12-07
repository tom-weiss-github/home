import string
import os
import sys
import subprocess
from optparse import OptionParser

ETAGS = "etags"



def AppendTags( tag_file_name, tag_files, verbose, no_execute ):
    cmd = "{0} --append --output={1} --language=c++ {2}".format( ETAGS, tag_file_name, tag_files )
    if( True == verbose ):
        print( "{0}".format( cmd ) )
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

( options, args ) = parser.parse_args()

if( 1 != len(args) ):
    print( "You must pass the destination of the tags file." )
    sys.exit( 0 )

dest = args[0]

print( "Create GNU Emacs TAGS." )
sys.stdout.flush()

tag_file_name = dest + "/TAGS"
tag_files = ""
count = 0
for (root, dirs, files) in os.walk( dest ):
    for name in files:
        a_file = str( os.path.join(root, name) )

        if( a_file.endswith( "h" )
            # or a_file.endswith( "cpp" )
            # or a_file.endswith( "inl" )                                 
             ):
            a_file = a_file.replace( "\\", "/" )
            tag_files += a_file
            tag_files += " "
            count += 1
            
        if( 30 == count ):
            AppendTags( tag_file_name, tag_files, options.verbose, options.no_execute )

            count = 0
            tag_files = ""

AppendTags( tag_file_name, tag_files, options.verbose, options.no_execute )

print( "Created tags file, {0}.".format( tag_file_name ) )
print( "Finished creating GNU Emacs TAGS." )
