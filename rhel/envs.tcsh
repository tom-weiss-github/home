
if( ! ($?LD_LIBRARY_PATH) ) then
    echo LD_LIBRARY_PATH is undefined.
else
    echo LD_LIBRARY_PATH $LD_LIBRARY_PATH
endif

if( ! ($?PATH) ) then
    echo PATH is undefined.
else
    echo PATH $PATH
endif

if( ! ($?C_INCLUDE_PATH) ) then
    echo C_INCLUDE_PATH is undefined.
else
    echo C_INCLUDE_PATH $C_INCLUDE_PATH
endif

if( ! ($?CPLUS_INCLUDE_PATH) ) then
    echo CPLUS_INCLUDE_PATH is undefined.
else
    echo CPLUS_INCLUDE_PATH $CPLUS_INCLUDE_PATH
endif

if( ! ($?PYTHONPATH) ) then
    echo PYTHONPATH is undefined.
else
    echo PYTHONPATH $PYTHONPATH
endif

if( ! ($?PYTHONHOME) ) then
    echo PYTHONHOME is undefined.
else
    echo PYTHONHOME $PYTHONHOME
endif

if( ! ($?SWIG_LIB) ) then
    echo SWIG_LIB is undefined.
else
    echo SWIG_LIB $SWIG_LIB
endif
