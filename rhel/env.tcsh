set DEBENV_ROOT=`git rev-parse --show-toplevel`
if( $DEBENV_ROOT == "" ) then
    echo "env.tcsh must be sourced from inside a git repository."
    exit 1
endif


set arch="x86-64"
set os="linux"
set DEBENV_CONFIG="$1"
set ext_path="$DEBENV_ROOT/ext/$os/$arch/release"
set build_path="$DEBENV_ROOT/build/$arch/$DEBENV_CONFIG"
set python_prefix="$ext_path"

alias se_path 'setenv PATH $build_path/bin:$ext_path/bin:$PATH'
alias se_ld_library_path 'setenv LD_LIBRARY_PATH $build_path/lib:$ext_path/lib:$LD_LIBRARY_PATH'
alias se_c_include_path 'setenv C_INCLUDE_PATH $python_prefix/include/python2.7:$C_INCLUDE_PATH'
alias se_cplus_include_path 'setenv CPLUS_INCLUDE_PATH $python_prefix/include/python2.7:$CPLUS_INCLUDE_PATH'
alias se_pythonpath 'setenv PYTHONPATH $build_path/python'
alias se_pythonhome 'setenv PYTHONHOME $ext_path'
alias se_swig_lib 'setenv SWIG_LIB $ext_path/share/swig/2.0.8'
alias setup_env 'se_path; se_ld_library_path; se_pythonpath; se_c_include_path; se_cplus_include_path; se_pythonhome; se_swig_lib'


alias save_env 'setenv DEBENV_PREV_PATH $PATH; setenv DEBENV_PREV_LD_LIBRARY_PATH $LD_LIBRARY_PATH; setenv DEBENV_PREV_C_INCLUDE_PATH $C_INCLUDE_PATH; setenv DEBENV_PREV_CPLUS_INCLUDE_PATH $CPLUS_INCLUDE_PATH'

alias restore_env 'setenv PATH $DEBENV_PREV_PATH; setenv LD_LIBRARY_PATH $DEBENV_PREV_LD_LIBRARY_PATH;  setenv C_INCLUDE_PATH $DEBENV_PREV_C_INCLUDE_PATH;  setenv CPLUS_INCLUDE_PATH $DEBENV_PREV_CPLUS_INCLUDE_PATH'

if( $DEBENV_CONFIG == "debug" || $DEBENV_CONFIG == "release" ) then
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
else if( $DEBENV_CONFIG == "reset" ) then
    if( "1" == $?DEBENV_ENGAGED ) then
        restore_env
        setenv DEBENV_ENGAGED "0"
    endif
else
    echo "Usage: source env.tcsh [debug|release|reset]"
endif

echo LD_LIBRARY_PATH $LD_LIBRARY_PATH
echo PATH $PATH
echo C_INCLUDE_PATH $C_INCLUDE_PATH
echo CPLUS_INCLUDE_PATH $CPLUS_INCLUDE_PATH
echo PYTHONPATH $PYTHONPATH
echo PYTHONHOME $PYTHONHOME
echo SWIG_LIB $SWIG_LIB
