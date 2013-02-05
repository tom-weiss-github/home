from optparse import OptionParser
import os
import subprocess
import sys

DEV_ROOT = "/home/debesys/dev-root"

def IsRepo( path ):
    if( True == os.path.exists( path ) ):
        os.chdir( path )
    else:
        return False

    cmd = [ "git", "rev-parse", "--show-toplevel" ]
    p = subprocess.Popen( cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE )
    (stdoutdata, stderrdata) = p.communicate()
    if( 0 == p.returncode ):
        return True
    else:
        return False


def command( cmd_sequence, verbose, no_execute ):
    if( True == verbose ):
        print( "{0}".format( str(cmd_sequence) ) )

    if( False == no_execute ):
        p = subprocess.Popen( cmd_sequence, stdin=subprocess.PIPE, stdout=subprocess.PIPE )
        (stdoutdata, stderrdata) = p.communicate()
        if( None != stdoutdata ):
            print( "{0}".format( stdoutdata ) )
        if( None != stderrdata ):
            print( "{0}".format( stderrdata ) )


def create_new_repo( repo_name, verbose, no_execute ):
    if( True == verbose ):
        print( "cd {0}".format( DEV_ROOT ) )
    if( False == no_execute ):
        os.chdir( DEV_ROOT )

    command( ["git",
              "clone",
              "git@github.com:tradingtechnologies/debesys.git",
              repo_name ], verbose, no_execute )

    if( True == verbose ):
        print( "cd {0}/{1}".format( DEV_ROOT, repo_name ) )
    if( False == no_execute ):
        os.chdir( "{0}/{1}".format( DEV_ROOT, repo_name ) )

    command( ["git", "submodule", "init"], verbose, no_execute )
    command( ["git", "submodule", "update"], verbose, no_execute )


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

parser.set_defaults( verbose=False )
parser.set_defaults( no_execute=False )

( options, args ) = parser.parse_args()

if( 1 != len(args) ):
    print( "You must pass the branch to create." )
    sys.exit( 0 )

branch_name = args[0]

# If the next repo doesn't exist, create it.
if( False == os.path.exists( DEV_ROOT + "/next" ) ):
    print( "Didn't find {0} repository, creating.".format( DEV_ROOT + "/next" ) )
    create_new_repo( "next", options.verbose, options.no_execute )

# Make sure it's a valid repository.
if( False == IsRepo( DEV_ROOT + "/next" ) ):
    print( "Didn't find a valid repository at {0}.".format( DEV_ROOT + "/next" ) )
    sys.exit( 1 )

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
