
#! /bin/csh -f
# Start of script compw3l.sh          R.E.Jones  90-06-12
#
echo Compile and place in library w3lib.a subroutine $1
#
# Example:  Compile subroutine q9ye32.f and put it into library w3lib
#
#           compw3l.sh q9ye32
#
#  Fortran compiler option for w3lib
#
. ./w3lib_fort_opts
#
rm    $1.o 2>/dev/null              # delete subroutine old object
echo "$F77 $FORT_OPTS -c $1.f"
$F77 $FORT_OPTS -c $1.f 
if [ $? -eq 0 ] ; then
	ar -rs w3lib.a $1.o  || exit 8  # put subr. just compiled into library w3lib.a
	ranlib w3lib.a || exit 8          # randomize library
	rm  $1.o              # delete subroutine new object
	echo Subroutine $1 was compiled and placed in library w3lib.a
else
	echo Compile error, subroutine $1 was not placed in library w3lib.a
fi
#
# End of script compw3l.sh
