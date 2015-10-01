#! /bin/csh
#
#   This script is a more or less generic library/executable
#   builder for SPICE products.  It assumes that it is executed
#   from one of the "product" directories in a tree that looks like
#   the one displayed below:
#
#                      package
#                         |
#                         |
#       +------+------+------+------+------+
#       |      |      |      |      |      | 
#     data    doc    etc    exe    lib    src
#                                          |
#                                          |
#                         +----------+----------+------- ... ------+
#                         |          |          |                  |
#                     product_1  product_2  product_3    ...   product_n
#
#   Here's the basic stategy:
#
#     1)  Compile all of the .f  and .for files in the current directory
#
#     2)  If there are not .pgm files in the current directory this
#         is assumed to be a library source directory.  The name
#         of the library is the same as the name of the product.
#         The library is placed in the "lib" directory in the tree
#         above.  The script is then done.
# 
#         If there are .pgm files and there were some .f or .for
#         files compiled the objects are gathered together in the
#         current directory into a library called locallib.a
#
#     3)  If any *.pgm files exist in the current directory, compile 
#         them and link their objects with locallib.a (if it exists), 
#         ../../spicelib.a and ../../support.a. The output 
#         executables have an empty extension.  The executables are
#         placed in the "exe" directory in the tree above.
#         
#   The environment variable TKF77OPTIONS containing compile options 
#   is optionally set. If it is set prior to executing this script,
#   those options are used. It it is not set, it is set within this
#   script as a local variable.
#
#   References:
#   ===========
#
#   "Unix Power Tools", page 11.02 
#      Use the "\" character to unalias a command temporarily.
#        
#   "A Practical Guide to the Unix System"
#
#   "The Unix C Shell Field Guide"
#
#   Change History:
#   ===============
#
#   Version 1.0.0  Dec 8, 1995   Bill Taber
#

#
#  What compile options do we want to use? If they were 
#  set somewhere else, use those values.
#
if ( $?TKF77OPTIONS ) then
   echo " "
   echo "      Using compile options: "
	echo "      $TKF77OPTIONS"
else
   set TKF77OPTIONS = "-C -u"
   echo " "
   echo "      Setting default compile options:"
	echo "      $TKF77OPTIONS"
endif

echo " "

#
#   Determine a provisional LIBRARY name.
#
   foreach item ( `pwd` )
		set LIBRARY = "../../lib/"$item:t
	end

#
#  Are there any *.f files that need to be compiled?
#
\ls *.f >& /dev/null

if ( $status == 0 ) then

   foreach SRCFILE ( *.f )
      echo "      Compiling: "   $SRCFILE
      f77 -c $TKF77OPTIONS $SRCFILE
   end

endif

#
#  Are there any *.for files that need to be compiled?
#
\ls *.for >& /dev/null

if ( $status == 0 ) then

   foreach SRCFILE ( *.for )
      echo "      Compiling: " $SRCFILE
      f77 -c $TKF77OPTIONS $SRCFILE
   end

endif

echo " "

#
#  If object files exist, we need to create an object library.
#

\ls *.pgm >& /dev/null

if ( $status == 0 ) then
	set LIBRARY = "locallib"
endif

\ls *.o >& /dev/null

if ( $status == 0 ) then

   echo "      Inserting objects in the library $LIBRARY ..."
   ar  crv $LIBRARY.a *.o
   \rm                *.o    
   echo " "

endif

#
#  If there are any main programs in the directory, compile
#  them. If they have their own locallib.a link with it in addition
#  to the default libraries.
#

\ls *.pgm >& /dev/null

if ( $status == 0 ) then

   echo " "

   foreach MAIN ( *.pgm )
      set TARGET = $MAIN:r.f
      set EXECUT = ../../exe/$MAIN:r
      \cp $MAIN  $TARGET
	
      echo "      Compiling and linking: " $MAIN
		
      if ( -e locallib.a ) then

         f77 -Bstatic -o $EXECUT $TKF77OPTIONS $TARGET     \
                                      locallib.a           \
                                      ../../lib/support.a  \
                                      ../../lib/spicelib.a
         \rm $TARGET
         \rm locallib.a 

      else

         echo "Compiling and linking: " $MAIN     
         f77 -Bstatic -o $EXECUT $TKF77OPTIONS $TARGET     \
                                      ../../lib/support.a  \
                                      ../../lib/spicelib.a
         \rm $TARGET

      endif

   end

endif

#
#  Cleanup.
#

echo " "

\ls *.o >& /dev/null

if ( $status == 0 ) then
   \rm *.o
endif


exit 0

	
	
