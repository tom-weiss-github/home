import string
import os
import sys
import subprocess

sys.path.append( "T:\\Development\\CoreBuilds\\Tools\\Scripts" )

from label import Label

def add_lci_envs( dest ):
    print( "Starting Fetch Hook to add LCI environment variables." )

    # product, revision, product_filter, doxygen_cfg_file, sleep
    lci_dict = { "btec"   :"x",
                 "ose"    :"x",
                 "sgx"    :"x",
                 "tocom"  :"x",
                 "core"   :"x"   }

    manifest_file_name = dest + "/manifest.cfg"

    if( False == os.path.exists( manifest_file_name ) ):
        print( "Unable to find %s." % manifest_file_name )
        return

    fd = open( manifest_file_name, 'r' )
    allLines = fd.readlines()

    for line in allLines:
        
        if not line:
            continue

        if( -1 != line.find( "source_btec" ) ):
            print( "LCI for BTEC." )

        elif( -1 != line.find( "source_ose" ) ):
            print( "LCI for OSE" )

        elif( -1 != line.find( "source_tocom" ) ):
            print( "LCI for TOCOM" )

        elif( -1 != line.find( "source_sgx" ) ):
            print( "LCI for SGX" )

        elif( -1 != line.find( "source_api" ) ):
            print( "LCI for CORE" )

        else:
            print( "LCI environment variables will not be configured." )
            
            

    fd.close()

    print( "Finished Fetch Hook to add LCI environment variables." )

def add_vrm_env( dest ):

    print( "Starting Fetch Hook to add PRODVRM environment variable." )

    vrmfilter = "*07.15.00*"
    cmd_file = dest + "/SetEnv.cmd"

    if( False == os.path.exists( cmd_file ) ):
        print( "Unable to find %s." % cmd_file )
        return
        
    fd = open( cmd_file, "r+" )
    allLines = fd.readlines()

    for line in allLines:
        
        if not line:
            continue

        if( -1 != line.find( "TT_OSE_VERSION" ) or
             -1 != line.find( "TT_TOCOM_VERSION" ) or
             -1 != line.find( "TT_SGX_VERSION" ) or 
             -1 != line.find( "TT_BTEC_VERSION" ) ):

            (env, sep, value) = line.partition( "=" )
            value = value.rstrip()
            if( 4 == len(value) ):
                vrmfilter = "*-0%s*" % value
            else:
                vrmfilter = "*%s*" % value


        # if( -1 != line.find( "TT_OM_SHARED_LABEL" ) ):
        #     (env, sep, value) = line.partition( "=" )

        #     if( -1 != value.find( "None" ) ):
        #         print( "Unable to create PRODVRM filter since TT_OM_SHARED_LABEL is None" )
        #     else:
        #         lbl = Label(value)
        #         vrmfilter = "*%02d.%02d.%02d*" % ( int(lbl.v), int(lbl.r), int(lbl.m) )


    fd.seek( 0, 2 )
    fd.write( "\nSET PRODVRM=%s\n" % vrmfilter )

    fd.close()    

    print( "Finished Fetch Hook to add PRODVRM environment variable (%s)." % (vrmfilter) )


def make_setenv_tcsh( dest ):
    print( "Starting Fetch Hook to create SetEnv.tcsh." )

    vrmfilter = "INVALID"
    cmd_file = dest + "/SetEnv.cmd"
    tcsh_file = dest + "/SetEnv.tcsh"

    if( False == os.path.exists( cmd_file ) ):
        print( "Unable to find %s." % cmd_file )
        return

    fd_c = open( cmd_file, "r" )
    fd_t = open( tcsh_file, 'w' )
    allLines = fd_c.readlines()

    for line in allLines:
        
        if not line:
            continue

        # Intent is to skip PATH/PYTHONPATH and only pick up the TT_
        # environment variables.
        if( -1 == line.find( "SET TT_" ) ):
            continue

        line = string.replace( line, "SET", "setenv" )
        line = string.replace( line, "=", " " )
        line = string.replace( line, "\\", "/" )
        fd_t.write( line )


    fd_c.close()
    fd_t.close()

    print( "Finished Fetch Hook to create SetEnv.tcsh." )


def create_emacs_tags( dest ):
    print( "Starting Fetch Hook to create GNU Emacs TAGS." )
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
                cmd = "c:/emacs-23.1/bin/etags.exe --append --output={0} --language=c++ {1}".format( tag_file_name,
                                                                                                     tag_files )
                p = subprocess.Popen( cmd, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE )
                (stdoutdata, stderrdata) = p.communicate()
                count = 0
                tag_files = ""

    print( "Created tags file, {0}.".format( tag_file_name ) )
    print( "Finished Fetch Hook to create GNU Emacs TAGS." )


def post_fetch( *args, **kwargs ):

    # None seem to be found for fetch.
    # for arg in args:
    #     print( "%s" % arg )

    if( "dest" in kwargs ):
        print( "Found destination as %s." % kwargs["dest"] )
        add_vrm_env( kwargs["dest"] )
        make_setenv_tcsh( kwargs["dest"] )
        create_emacs_tags( kwargs["dest"] )
    else:
        print( "Failed to find 'dest', nothing meaningful executed." )

    # Example of printing source component information, object is SourceComponent, defined
    # in fetch.
    # if( "src_comps" in kwargs ):
    #     for src_comp in kwargs["src_comps"]:
    #         print( "%s %s %s %s" % (src_comp.category, src_comp.component, src_comp.version, src_comp.label) )

    


if( __name__ == "__main__" ):
    create_emacs_tags( "c:/tt-dev/ose-pcr-204937-audit" )
