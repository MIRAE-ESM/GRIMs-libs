inimon=$1
###fmon=` expr $2 - 1 `
fmon=$2
yy=` expr $inimon / 100 `
if [ $yy -lt 100 ];then yy=` expr 1900 + $yy `;fi
mm=` expr $inimon % 100 `
#save for print
y=$yy
m=$mm

      m1=` expr $mm + $fmon - 1` 
      m2=` expr $mm + $fmon ` 
      dy=` expr $m1 / 12 `
      yy=` expr $yy + $dy `
      mm=` expr $m2 % 12 `
      if [ $m2 -le 0 ];then
         mm=` expr $mm + 12 ` 
         yy=` expr $yy - 1 `
      fi
      if [ $mm -eq 0 ];then mm=12;fi
      if [ $mm -lt 10 ];then mm=0$mm;fi

echo $yy$mm 
echo " Input with year=$y, month=$m "
echo " and fcstmonth=$fmon "
echo " Validate  at year=$yy, month=$mm "
