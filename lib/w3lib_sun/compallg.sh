set -a
# compile all the subroutines in the w3lib.a library R.E.JONES  93-10-05
# compile just the subroutines for unpkgrb1.f (grib file unpacker)

# sb=0,1,2  0..NCAR, 1..Henry, 2..NCAR-SUNSPARC+1  SBYTE/SBYTES
# gb=0,1,2  0..NCAR, 1..Henry, 2..NCAR-SUNSPARC+1  GBYTE/GBYTES
#  
gb=0
cp ${gb}gbyte.f gbyte.f
cp ${gb}gbytes.f gbytes.f
./compw3l.sh gbyte
./compw3l.sh gbytes
#
sb=1
cp ${sb}sbyte.f sbyte.f
cp ${sb}sbytes.f sbytes.f
./compw3l.sh sbyte
./compw3l.sh sbytes
#
./compw3l.sh aea
./compw3l.sh datimx
./compw3l.sh iw3jdn
./compw3l.sh iw3pds
./compw3l.sh w3fi01
./compw3l.sh w3fi63
./compw3l.sh w3fp11
./compw3l.sh w3tagb
./compw3l.sh xmovex
./compw3l.sh w3fi04
./compw3l.sh w3fi83
./compw3l.sh w3fs26
# End of compile of all subroutines into w3lib.a

# add pack routines
./compw3l.sh w3pack
#
# New addition for Mark Iredells grib read routines
#
./compw3l.sh unpindx
./compw3l.sh getgir
./compw3l.sh assign
./compw3l.sh baread
./compw3l.sh ixgb
./compw3l.sh skgb
./compw3l.sh rdgb
./compw3l.sh getgbss
./compw3l.sh pdseup
./compw3l.sh pdsens
