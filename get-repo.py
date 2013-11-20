import argparse
import os
import subprocess
import sys
import atexit
import shutil

DEV_ROOT = "{0}/dev-root".format(os.getenv("HOME"))

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


descr = "\nget-repo.py branch"
parser = argparse.ArgumentParser(description=descr)

parser.add_argument( "-v", "--verbose", default=False,
                   action="store_true", dest="verbose",
                   help="(Optional) Run in verbose mode." )

parser.add_argument( "-n", "--no-execute", default=False,
                     action="store_true", dest="no_execute",
                     help="(Optional) Commands will not be executed.  "
                     "When used with -v, this allows users to see what "
                     "actions the script will perform without actually "
                     "executing them." )

parser.add_argument("branch", help="The new branch name")

args = parser.parse_args()

# Check for a pidfile to see if the daemon already runs
pidfile = "{0}/get-repo.pid".format(DEV_ROOT)
try:
    pf = file(pidfile,'r')
    pid = int(pf.read().strip())
    pf.close()
except IOError:
    pid = None

if pid:
    message = "pidfile %s already exists, already running?\n"
    sys.stderr.write(message % pidfile)
    sys.exit(1)

branch_name = args.branch

new_repo = DEV_ROOT + "/" + branch_name
cached_repo = DEV_ROOT + "/next"

if( os.path.exists( new_repo ) ):
    print( "Error:  {0} already exists.".format( new_repo ) )
    sys.exit( 1 )

if( False == os.path.exists( cached_repo ) ):
    print( "Didn't find {0} repository, creating.".format( cached_repo ) )
    create_new_repo( "next", args.verbose, args.no_execute )

if( False == args.no_execute and
    False == IsRepo( cached_repo ) ):
    print( "Didn't find a valid repository at {0}.".format( cached_repo ) )
    sys.exit( 1 )



#
# For some reason, this technique of copying the repo causes problems like
# unexplained differences in ext files and very long git status execution.
# Perhaps try cloning from next and then set the origin back to github.
# Running strace git status is showing me that at some point during the
# command it get stuck for a long time.  Not clear what's causing that.
#

# To use this method, uncomment everything below to the end.
# if( True == args.verbose ):
#     print_with_border( "cd {0}".format( cached_repo ), "-" )
# if( False == args.no_execute ):
#     os.chdir( cached_repo )

# command( [ "git", "checkout", "master"], True, args.verbose, args.no_execute )
# command( [ "git", "fetch" ], True, args.verbose, args.no_execute )
# command( [ "git", "merge", "origin/master" ], True, args.verbose, args.no_execute )
# command( [ "git", "submodule", "update" ], True, args.verbose, args.no_execute )
# command( [ "cp", "--recursive", cached_repo, new_repo ], True, args.verbose, args.no_execute )

# if( True == args.verbose ):
#     print_with_border( "cd {0}".format( new_repo ), "-" )
# if( False == args.no_execute ):
#     os.chdir( new_repo )

# if( True == BranchExists( branch_name, new_repo, args.verbose, args.no_execute ) ):
#     print( "Error: {0} already exists in {1}.".format( branch_name, new_repo ) )
#     sys.exit( 1 )

# command( [ "git", "branch", branch_name ], True, args.verbose, args.no_execute )
# command( [ "git", "checkout", branch_name ], True, args.verbose, args.no_execute )

# print_with_border("Repository {0} is now ready to use.".format(new_repo), "X")
# print("")




if( True == args.verbose ):
    print_with_border( "mv {0} {1}".format( cached_repo, new_repo ), "-" )
if( False == args.no_execute ):
    os.rename( cached_repo, new_repo )

if( True == args.verbose ):
    print_with_border( "cd {0}".format( new_repo ), "-" )
if( False == args.no_execute ):
    os.chdir( new_repo )

if( True == BranchExists( branch_name, new_repo, args.verbose, args.no_execute ) ):
    print( "Error: {0} already exists in {1}.".format( branch_name, new_repo ) )
    sys.exit( 1 )

command( [ "git", "checkout", "master"], True, args.verbose, args.no_execute )
command( [ "git", "fetch" ], True, args.verbose, args.no_execute )
command( [ "git", "merge", "origin/master" ], True, args.verbose, args.no_execute )
command( [ "git", "submodule", "update" ], True, args.verbose, args.no_execute )
command( [ "git", "branch", branch_name ], True, args.verbose, args.no_execute )
command( [ "git", "checkout", branch_name ], True, args.verbose, args.no_execute )


print_with_border( "Repository {0} is now ready to use, creating {1} for future use.".format( new_repo, cached_repo ), "X" )
print( "" )

if( False == args.no_execute and
    True == os.path.exists( cached_repo ) ):
    print( "Error: {0} already exists.".format( cached_repo ) )
    sys.exit( 1 )

try:
    pid = os.fork()
    if pid > 0:
        # exit first parent
        sys.exit(0)
except OSError, e:
    sys.stderr.write("fork #1 failed: %d (%s)\n" % (e.errno, e.strerror))
    sys.exit(1)

# decouple from parent environment
os.chdir("/")
os.setsid()
# Keep the current umask value.

# do second fork
try:
    pid = os.fork()
    if pid > 0:
        # exit from second parent
        sys.exit(0)
except OSError, e:
    sys.stderr.write("fork #2 failed: %d (%s)\n" % (e.errno, e.strerror))
    sys.exit(1)

# redirect standard file descriptors
sys.stdout.flush()
sys.stderr.flush()
si = file("/dev/null", 'r')
so = file("/dev/null", 'a+')
se = file("/dev/null", 'a+', 0)
os.dup2(si.fileno(), sys.stdin.fileno())
os.dup2(so.fileno(), sys.stdout.fileno())
os.dup2(se.fileno(), sys.stderr.fileno())

# write pidfile
atexit.register( os.remove, pidfile )
pid = str(os.getpid())
file(pidfile,'w+').write("%s\n" % pid)
create_new_repo( "next", args.verbose, args.no_execute )

sys.exit( 0 )


# Figure out what to do with slashes in branch names.  Need to change the slash for directory operations.

# Add option about shared branch.  Instead of "git branch <br>", and "git checkout <br>", do "git
# checkout -t origin/<br>".

# For shared branch, the branch must already exist instead of not exist.


# Explored cloning a local repo and then changing the remote.  This worked fine
# for the main repo, but not for the submodules.  Those seem to initialize from
# the remote site.  In order to resolve this, I tried to manually clone the submodules
# with a depth of 1.  In the end this didn't show much improvement over the current
# method.
# (from /home/debesys/dev-root)
# git clone --no-hardlinks /home/debesys/dev-root/next new
# cd next
# git config --get remote.origin.url
# cd new
# git remote rm origin
# git remote add origin (next's url from above command)
# Script for manually cloning submodules:
#!/bin/bash
# git submodule init
# for i in $(git submodule | sed -e 's/.* //'); do
#     spath=$(git config -f .gitmodules --get submodule.$i.path)
#     surl=$(git config -f .gitmodules --get submodule.$i.url)
#     git clone --depth 1 $surl $spath
# done
# git submodule update
