from optparse import OptionParser
import os
import subprocess
import sys

DEV_ROOT = "/home/debesys/dev-root"

def print_with_border( message, border_character ):
    border = border_character * len(message)
    print( border )
    print( "{0}".format( message ) )
    print( border )


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


def BranchExists( branch, path, verbose, no_execute ):
    if( True == no_execute ):
        return( False )

    os.chdir( path )
    output = command( ["git", "branch", "-a"], False, verbose, no_execute )
    branch_full_name = "remotes/origin/{0}".format( branch )
    for item in output.split():
        if( -1 != item.find( branch_full_name ) ):
            return( True )
    return( False )


def command( cmd_sequence, print_output, verbose, no_execute ):
    if( True == verbose ):
        command_as_string = ""
        for item in cmd_sequence:
            command_as_string += item + " "
        command_as_string = command_as_string[0:len(command_as_string)-1]
        print_with_border( command_as_string, "-" )

    stdoutdata = ""
    if( False == no_execute ):
        p = subprocess.Popen( cmd_sequence, stdin=subprocess.PIPE, stdout=subprocess.PIPE )
        (stdoutdata, stderrdata) = p.communicate()
        if( None != stdoutdata and True == print_output ):
            print( "{0}".format( stdoutdata ) )
        if( None != stderrdata and True == print_output ):
            print( "{0}".format( stderrdata ) )

    return( stdoutdata )


def create_new_repo( repo_name, verbose, no_execute ):
    if( True == verbose ):
        print_with_border( "cd {0}".format( DEV_ROOT ), "-" )
    if( False == no_execute ):
        os.chdir( DEV_ROOT )

    command( ["git",
              "clone",
              "git@github.com:tradingtechnologies/debesys.git",
              repo_name ], True, verbose, no_execute )

    if( True == verbose ):
        print_with_border( "cd {0}/{1}".format( DEV_ROOT, repo_name ), "-" )
    if( False == no_execute ):
        os.chdir( "{0}/{1}".format( DEV_ROOT, repo_name ) )

    command( ["git", "submodule", "init"], True, verbose, no_execute )
    command( ["git", "submodule", "update"], True, verbose, no_execute )


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

new_repo = DEV_ROOT + "/" + branch_name
cached_repo = DEV_ROOT + "/next"

if( os.path.exists( new_repo ) ):
    print( "Error:  {0} already exists.".format( new_repo ) )
    sys.exit( 1 )

if( False == os.path.exists( cached_repo ) ):
    print( "Didn't find {0} repository, creating.".format( cached_repo ) )
    create_new_repo( "next", options.verbose, options.no_execute )

if( False == options.no_execute and
    False == IsRepo( cached_repo ) ):
    print( "Didn't find a valid repository at {0}.".format( cached_repo ) )
    sys.exit( 1 )

if( True == options.verbose ):
    print_with_border( "rename {0} {1}".format( cached_repo, new_repo ), "-" )
if( False == options.no_execute ):
    os.rename( cached_repo, new_repo )

if( True == options.verbose ):
    print_with_border( "cd {0}".format( new_repo ), "-" )
if( False == options.no_execute ):
    os.chdir( new_repo )

if( True == BranchExists( branch_name, new_repo, options.verbose, options.no_execute ) ):
    print( "Error: {0} already exists in {1}.".format( branch_name, new_repo ) )
    sys.exit( 1 )

command( [ "git", "checkout", "master"], True, options.verbose, options.no_execute )
command( [ "git", "fetch" ], True, options.verbose, options.no_execute )
command( [ "git", "merge", "origin/master" ], True, options.verbose, options.no_execute )
command( [ "git", "submodule", "update" ], True, options.verbose, options.no_execute )
command( [ "git", "branch", branch_name ], True, options.verbose, options.no_execute )
command( [ "git", "checkout", branch_name ], True, options.verbose, options.no_execute )


print_with_border( "Repository {0} is now ready to use, creating {1} for future use.".format( new_repo, cached_repo ), "X" )
print( "" )

if( False == options.no_execute and
    True == os.path.exists( cached_repo ) ):
    print( "Error: {0} already exists.".format( cached_repo ) )
    sys.exit( 1 )

create_new_repo( "next", options.verbose, options.no_execute )

sys.exit( 0 )
