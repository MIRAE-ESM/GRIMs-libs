#!/bin/sh
#
curr_dir=`pwd`
mkdir Sdiff
if [ $# -ne 0 ]; then
	cmp_file=$1
else
	cmp_file=`/bin/ls *.f`
fi
cmp_dir="/global/disk1/LIBS/`echo $curr_dir|awk -F'libs' '{print $2}'`"

for file in $cmp_file
do
	dif_file=Sdiff/$file.diff
	sdiff -s -w 160 $file $cmp_dir/$file | fgrep -v \! | fgrep -v '\^c' > $dif_file
	size=`/bin/ls -l $dif_file | awk '{print $5}'`
	if [ $size -gt 20 ]; then
		echo $cmd >> $dif_file
		echo "$dif_file created"
	else
		/bin/rm -f $dif_file
	fi
done
