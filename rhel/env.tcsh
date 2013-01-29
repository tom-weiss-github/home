set root=`pwd | cut -d "/" -f 1,2,3,4,5`
set arch="x86-64"
set os="linux"

set config="$1"
set build_path="$root/build/$arch/$config"
set ext_path="$root/ext/$os/$arch/release"


alias se_path 'setenv PATH $build_path/bin:$PATH'
alias se_ld_library_path 'setenv LD_LIBRARY_PATH $build_path/lib:$ext_path/lib:$LD_LIBRARY_PATH'
alias se_c_include_path 'setenv C_INCLUDE_PATH `python2.7-config --prefix`/include/python2.7:$C_INCLUDE_PATH'
alias se_cplus_include_path 'setenv CPLUS_INCLUDE_PATH  `python2.7-config --prefix`/include/python2.7:$CPLUS_INCLUDE_PATH'
alias se_pythonpath 'setenv PYTHONPATH $ext_path/lib/python2.7/site-packages:$build_path/python'
alias setup_env 'se_path; se_ld_library_path; se_pythonpath; se_c_include_path; se_cplus_include_path;'


alias save_env 'setenv DEBENV_PREV_PATH $PATH; setenv DEBENV_PREV_LD_LIBRARY_PATH $LD_LIBRARY_PATH; setenv DEBENV_PREV_C_INCLUDE_PATH $C_INCLUDE_PATH; setenv DEBENV_PREV_CPLUS_INCLUDE_PATH $CPLUS_INCLUDE_PATH; setenv DEBENV_PREV_PYTHONPATH $PYTHONPATH'

alias restore_env 'setenv PATH $DEBENV_PREV_PATH; setenv LD_LIBRARY_PATH $DEBENV_PREV_LD_LIBRARY_PATH;  setenv C_INCLUDE_PATH $DEBENV_PREV_C_INCLUDE_PATH;  setenv CPLUS_INCLUDE_PATH $DEBENV_PREV_CPLUS_INCLUDE_PATH; setenv PYTHONPATH $DEBENV_PREV_PYTHONPATH'

if( $1 == "debug" || $1 == "release" ) then
    # The $?DEBENV_ENGAGED syntax over
    #     $DEBENV_ENGAGED prevents an error if
    # DEBENV_ENGAGED is not defined.
    if( "1" == $?DEBENV_ENGAGED ) then
        restore_env
    else
        save_env
    endif
    setup_env
    setenv DEBENV_ENGAGED "1"
else if( $1 == "reset" ) then
    if( "1" == $?DEBENV_ENGAGED ) then
        restore_env
        setenv DEBENV_ENGAGED "0"
    endif
else
    echo "Usage: source env.tcsh [debug|release|reset]"
endif

echo LD_LIBRARY_PATH $LD_LIBRARY_PATH;
echo PATH $PATH;
echo C_INCLUDE_PATH $C_INCLUDE_PATH;
echo CPLUS_INCLUDE_PATH $CPLUS_INCLUDE_PATH;
echo PYTHONPATH $PYTHONPATH
