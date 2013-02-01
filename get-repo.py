from optparse import OptionParser
import os
import subprocess

def command( cmd_sequence ):
    p = subprocess.Popen( cmd_sequence, stdin=subprocess.PIPE, stdout=subprocess.PIPE )
    (stdoutdata, stderrdata) = p.communicate()
    if( None != stdoutdata ):
        print( "{0}".format( stdoutdata ) )
    if( None != stderrdata ):
        print( "{0}".format( stderrdata ) )

def create_new_repo( repo_name ):
    os.chdir( "/home/debesys/dev-root" )

    command( ["git",
              "clone",
              "git@github.com:tradingtechnologies/debesys.git",
              repo_name ] )
    os.chdir( "/home/debesys/dev-root/{0}".format( repo_name ) )
    command( ["git", "submodule", "init"] )
    command( ["git", "submodule", "update"] )


usage = "\n%prog branch"
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
    print( "You must pass the branch to create." )
    sys.exit( 0 )

branch_name = args[0]

create_new_repo( "areallyunlikelyduplicate" )

# Locate the ~/dev-root/next folder and verify it's a repo.
# Rename ~/dev-root/next with the branch argument.
# cd into ~/dev-root/branch
# fetch latest master and submodule update
# create branch
# checkout the branch

# Start downloading the next repo.
# cd ~/dev-root
# git clone git@github.com:tradingtechnologies/debesys.git next
# git submodule init
# git submodule update
