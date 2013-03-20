#!/usr/bin/env python

from optparse import OptionParser
import os
import sys

def print_with_border( message, border_character ):
    border = border_character * len(message)
    print( border )
    print( "{0}".format( message ) )
    print( border )


usage = "\n%prog [directory]" + \
    "\nTraverses a directory and organizes all files by extension." \
    "\nEach extension's accumulated file size and optionally all files" \
    "\nare displayed.  This program is intended to be used to create" \
    "\nfind/grep filters for large source code trees."
parser = OptionParser( usage=usage )

parser.add_option( "-v", "--verbose",
                   action="store_true", dest="verbose",
                   help="(Optional) Run in verbose mode." )
parser.add_option( "--print-all-files",
                   action="store_true", dest="print_all_files",
                   help="(Optional) Print all files in each extension, default is false." )

parser.set_defaults( verbose=False )
parser.set_defaults( print_all_files=False )

( options, args ) = parser.parse_args()

root_directory = ""
if( 1 == len(args) ):
    root_directory = args[0]
else:
    root_directory = os.getcwd()

if( True == options.verbose ):
    print( "Searching '{0}'.".format( root_directory ) )

# extension : list of (size, absolute path) tuples
file_extension_dict = dict()

# total_size : list of extensions
extension_size_dict = dict()

for( root, dirs, files ) in os.walk( root_directory ):
    for name in files:
        absolute_path = str( os.path.join( root, name ) )

        file_info = os.lstat( absolute_path )
        file_size =  file_info.st_size
        file_extension = name.split(".")[-1]

        if file_extension not in file_extension_dict:
            file_extension_dict[file_extension] = list()
        file_extension_dict[file_extension].append( (file_size, absolute_path) )

for (ext, file_info) in file_extension_dict.iteritems():
    accumulated_size = 0
    for index in range( 0, len(file_info) ):
        accumulated_size = accumulated_size + file_info[index][0]

    if accumulated_size not in extension_size_dict:
        extension_size_dict[accumulated_size] = list()
    extension_size_dict[accumulated_size].append( ext )


for( size ) in sorted( extension_size_dict.keys() ):
    for ext_index in range( 0, len( extension_size_dict[size] ) ):
        extension = extension_size_dict[size][ext_index]

        if extension not in file_extension_dict:
            print( "Error: extension {0} not in file extension dictionary.".format( extension ) )
            print( "file extension dictionary: {0}".format( str(file_extension_dict) ) )
            sys.exit(1)
        flist = file_extension_dict[extension]

        print_with_border( "extension: {0}".format(extension), "*" )
        print( "Files: {0}".format( len(flist) ) )
        print( "Accumulated size: {0}".format( size ) )
        if( True == options.print_all_files ):
            for index in range( 0, len(flist) ):
                print( "[{0}] {1} {2}".format( index+1, flist[index][0], flist[index][1] ) )
        print( "" )
