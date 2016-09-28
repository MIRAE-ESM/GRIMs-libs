#!/bin/sh
#
# usage::  grib2ctl.s pgb.ft06
#
host=`hostname | cut -d . -f1`
if [ $host = global ];then
host_head=''
else
host_head='/global'
fi
DDIR=`pwd`
cd $DDIR
filen=$1
$host_head/disk1/LIBS/etc/grib2ctl.pl $1 > $1.ctl
gribmap -i $1.ctl -0
