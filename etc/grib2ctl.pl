#!/usr/bin/perl -w
#   makes a GrADS control file for grib files
#
#   requires wgrib and Perl5
#
#   usage: grib2ctl [options] [grib file] [optional index file] >[control file]
#
#   note: this script does not make the index file .. you have to run gribmap
#
#   Analyses: (using initial time)
#
#      $ grib2ctl.pl example.grib >example.ctl
#      $ gribmap -i example.ctl -0
#
#   Forecasts: (using verifiation time)
#
#      $ grib2ctl.pl -verf example.grib >example.ctl
#      $ gribmap -i example.ctl
#
#   bugs:
#         many
#	  will fail under number of situations
#         finite number of NCEP grids are supported
#
# requires wgrib 1.6.0 or higher 
# wesley ebisuzaki, http://wesley.wwb.noaa.gov/grib2ctl.html

$version="0.9.12.5p31";

$wflag="";
$file="";
$index="";
$prs="prs";
$suffix="";
$z_order="prs";
$model="MRF";

foreach $_ (@ARGV) {
   SWITCH: {
      /^-verf/ && do { $wflag="$wflag -verf" ; last SWITCH; };
      /^-ncep_opn/ && do { $wflag="$wflag -ncep_opn" ; last SWITCH; };
      /^-ncep_rean/ && do { $wflag="$wflag -ncep_rean" ; last SWITCH; };
      /^-no_prs/ && do { $prs="" ; last SWITCH; };
      /^-no_suffix/ && do { $suffix="no" ; last SWITCH; };
      /^-rev_z/ && do { $z_order="theta"; last SWITCH; };
      /^-eta/ && do { $model="ETA"; last SWITCH; };
      /^-mrf/ && do { $model="MRF"; last SWITCH; };
      /^-/ && do { print STDERR "unknown option: $_\n"; exit 8; };
      if ($file eq "") {
         $file="$_";
      }
      else {
         $index="$_";
      }
   }
}

if ("$file" eq "") {
   if ($#ARGV >= 0) {
      print STDERR "*** missing grib file ***\n\n\n";
   }
   print STDERR "$0 $version  wesley ebisuzaki\n";
   print STDERR " makes a Grads control file for grib files\n";
   print STDERR " usage: $0 [options] [grib file] [optional index file] >[ctl file]\n";
   print STDERR " -ncep_opn       .. use NCEP opn grib table for T62 NCEP fields\n";
   print STDERR " -ncep_rean      .. use NCEP reanalysis grib table for T62 NCEP fields\n";
   print STDERR " -verf           .. use forecast verification times\n";
   print STDERR " -no_prs         .. no prs suffix on variable name\n";
   print STDERR " -no_suffix      .. no suffix on variable name\n";
   print STDERR " -rev_z          .. for reversed vertical coordinates like theta\n";
   print STDERR " -eta            .. eta model level\n";
   print STDERR " -mrf            .. mrf model level (default)\n";
   exit 8;
}

if (-d "c:\\") {
   $ListA="c:\\g$$.tmp";
   $TmpFile="c:\\h$$.tmp";
   unlink ($ListA, $TmpFile);
   $sys="win";
}
else {
   $ListA="/tmp/g$$.tmp";
   $TmpFile="/dev/null";
   unlink $ListA;
   $sys="unix";
}

# inventory of All records

system "wgrib $wflag -v $file >$ListA";

if ( ! -s $ListA ) {
    print STDERR "Big problem:\n";
    print STDERR "  either $file is missing or not a grib file\n";
    print STDERR "  or wgrib is not on your path\n";
    exit 8;
}

# make table of dates and variables

open (FileDate, "<$ListA");
while (defined($_ = <FileDate>)) {

   # date table

   $_ =~ s/^.*D=//;
   $d=substr($_, 0, 10);
   $dates{$d}="";

   # variable/level list
   @Fld = split(':', $_, 99);
   $kpds=substr($Fld[3],5);
   ($kpds5,$kpds6,$kpds7) = split(/,/,$kpds);
   $varname = "$Fld[1]:$kpds6";
   if (defined $flevels{$varname}) {
      if (!($flevels{$varname} =~ / $kpds7 /)) {
         $flevels{$varname} .= "$kpds7 ";
      }
   }
   else {
      $flevels{$varname} = " $kpds7 ";
      $fcomments{$varname} = "$kpds5:$Fld[$#Fld]";
   }
}
close (FileDate);
@sdates=sort keys(%dates);

$ntime=$#sdates + 1;
$time=$sdates[0];
$year = substr($time,0,4);
$mo = substr($time,4,2);
$day = substr($time,6,2);
$hour = substr($time,8,2);
$month=substr("janfebmaraprmayjunjulaugsepoctnovdec",$mo*3-3,3);

if ($ntime > 1) {
    $year1 = substr($sdates[1],0,4);
    $mo1 = substr($sdates[1],4,2);
    $day1 = substr($sdates[1],6,2);
    $hour1 = substr($sdates[1],8,2);
}

# ---------------intro------------------------------------

if ("$index" eq "" ) {$index="$file.idx";}
if ($sys eq "unix") {
   $caret1 = (substr($file,0,1) eq "/") ? "" : '^';
   $caret2 = (substr($index,0,1) eq "/") ? "" : '^';
}
else {
   $caret1 = (substr($file,1,1) eq ":") ? "" : '^';
   $caret2 = (substr($index,1,1) eq ":") ? "" : '^';
}
print "dset $caret1$file\nindex $caret2$index\n";
print "undef 9.999E+20\ntitle $file\n*  produced by grib2ctl v$version\n";

# ------------------- grid -----------------------
$griddef=`wgrib $wflag -V $file -d 1 -o $TmpFile`;
($grid = $griddef) =~ s/^.*grid=//;
$grid =~ s/ .*//s;

print "dtype grib $grid\n";

$_="$griddef";
if (/  latlon: /) {
   / lat  (\S*) to (\S*) by (\S*) /;
   $lat0=$1;
   $lat1=$2;
   $dlat=$3;

   / long (\S*) to (\S*) by (\S*), \((\S*) x (\S*)\)/;
   $lon0=$1;
   # $lon1=$2;
   $dlon=$3;
   $nx =$4;
   $ny =$5;

   if ($lat0 > $lat1) {
      print "options yrev\n";
      print "ydef $ny linear $lat1 ", abs($dlat), "\n"
   }
   else {
      print "ydef $ny linear $lat0 ", abs($dlat), "\n"
   }
   print "xdef $nx linear $lon0 $dlon\n";
}
elsif ($grid == 5) {
   print "pdef 53 57 nps 27 49 -105 190.5\n";
   print "xdef 161 linear -140 0.5\n";
   print "ydef 81 linear 20 0.5\n";
}
elsif ($grid == 6) {
   print "pdef 53 45 nps 27 49 -105 190.5\n";
   print "xdef 161 linear -140 0.5\n";
   print "ydef 81 linear 20 0.5\n";
}
elsif ($grid == 87) {
   print "pdef 81 62 nps 31.9 112.53 -105 68.513\n";
   print "xdef 161 linear -140 0.5\n";
   print "ydef 81 linear 20 0.5\n";
}
elsif ($grid == 101) {
   print "pdef 113 91 nps 58.5 92.5 -105 91.452\n";
   print "xdef 161 linear -140 0.5\n";
   print "ydef 81 linear 20 0.5\n";
}
elsif ($grid == 104) {
   print "pdef 147 110 nps 75.5 109.5 -105 90.75464\n";
   print "xdef 161 linear -140 0.5\n";
   print "ydef 81 linear 20 0.5\n";
}
elsif ($grid == 105) {
   print "pdef 83 83 nps 40.5 88.5  -105 90.75464\n";
   print "xdef 161 linear -140 0.5\n";
   print "ydef 81 linear 20 0.5\n";
}
elsif ($grid == 106) {
   print "pdef 165 117 nps 80 176 -105 45.37732\n";
   print "xdef 161 linear -140 0.5\n";
   print "ydef 81 linear 20 0.5\n";
}
elsif ($grid == 107) {
   print "pdef 120 92 nps 46 167  -105 45.37732\n";
   print "xdef 161 linear -140 0.5\n";
   print "ydef 81 linear 20 0.5\n";
}
elsif ($grid == 201) {
   print "pdef 65 65 nps 33 33 -105 381\n";
   print "xdef 180 linear -180 2\n";
   print "ydef 51 linear -10 2\n";
}
elsif ($grid == 202) {
   print "pdef 65 43 nps 33 45 -105 190.5\n";
   print "xdef 91 linear -200 2\n";
   print "ydef 41 linear 10 2\n";
}
elsif ($grid == 203) {
   print "pdef  45 39 nps 27 37 -150 190.5\n";
   print "xdef 103 linear -250 2\n";
   print "ydef 33 linear 26 2\n";
}
elsif ($grid == 205) {
   print "pdef  45 39 nps 27 57 -60 190.5\n";
   print "xdef 50 linear -120 2\n";
   print "ydef 46 linear 0 2\n";
}
elsif ($grid == 207) {
   print "pdef 49 35 nps 25 51 -150 95.25\n";
   print "xdef 51 linear -200 2\n";
   print "ydef 30 linear 45 1\n";
}
elsif ($grid == 211) {
   # awips labert conformal
   print "pdef 93 65 lcc 12.19 -133.459 1 1 25 25 -95 81270.5 81270.5\n";
   print "xdef 161 linear -140 0.5\n";
   print "ydef 81 linear 20 0.5\n";
}
elsif ($grid == 212) {
   # awips labert conformal
   print "pdef 185 129 lcc 35.0 -95.0 105 49 25 25 -95 40635 40635\n";
   print "xdef 181 linear -140 0.5\n";
   print "ydef  91 linear  15 0.5\n";
}
elsif ($grid == 213) {
   print "pdef  129 85 nps 65 89 -105 95.25\n";
   print "xdef 170 linear -190 1\n";
   print "ydef 81 linear 10 1\n";
}
elsif ($grid == 215) {
   # labert conformal
   print "pdef 369 257 lcc 12.19 -133.46 1 1 25 25 -95 20318 20318\n";
   print "xdef 289 linear -136 0.25\n";
   print "ydef  157 linear 18 0.25\n";
}
elsif ($grid == 216) {
   print "pdef 147 110 nps 75.5 109.5 -105 91.452\n";
   print "xdef 181 linear -180 1\n";
   print "ydef 91 linear 0 1\n";
}
elsif ($grid == 218) {
   # awips labert conformal
   print "pdef 737 513 lcc 12.19 -133.459 1 1 25 25 -95 10.159 10.159\n";
   print "xdef 161 linear -140 0.5\n";
   print "ydef 81 linear 20 0.5\n";
}
elsif ($grid == 240) {
   # nps usa
   print "pdef 1160 880 nps 441 1601 255 4.763\n";
   print "xdef 801 linear -130 0.1\n";
   print "ydef 401 linear 20 0.1\n";
}
elsif ($grid == 241) {
   print "pdef 386 293 nps 147.315 534.0 -105 14.2875\n";
   print "xdef 161 linear -140 0.5\n";
   print "ydef 81 linear 20 0.5\n";
}
else {
   # unknown grid

   $_ = $griddef;
   GRD: {

      / polar stereo: Lat1 16.125000 Long1 234.983000 Orient -100.0/ && do {
         print "options yrev\n";
         print "pdef 129 86 nps 64 136 -100 60\n";
         print "xdef 720 linear  0 0.5\n";
         print "ydef 148 linear 16 0.5\n";
	 last GRD; };

      / polar stereo: Lat1 -4.860000 Long1 -122.614000 Orient -80.000000/ && do {
         print "pdef 49 51 nps 24 26 -80 381\n";
         print "xdef 144 linear 0 2.5\n";
         print "ydef 45 linear -20 2.5\n";
         last GRD; };

      / Lambert Conf:.* Lov 265.*\(151 x 113\)/s && do {
         print "options yrev\n";
         print "pdef 151 113 lcc 16.281 233.8622 1 1 25 25 265 40635 40635\n";
         print "xdef 141 linear -130 0.5\n";
         print "ydef 71 linear 20 0.5\n";
         last GRD; };

      / gaussian:/ && do {
         / lat  (\S*) to (\S*)/;
         $lat0=$1;
         $lat1=$2;

         / long (\S*) to (\S*) by (\S*), \((\S*) x (\S*)\)/;
         $lon0=$1;
         # $lon1=$2;
         $dlon=$3;
         $nx =$4;
         $ny =$5;
         $dlon = 360 / $nx;

         if ($lat0 > $lat1) {
            print "options yrev\n";
         }
         print "xdef $nx linear $lon0 $dlon\n";
         print "ydef $ny levels\n";
      
         if ($ny == 94) { 
         print "-88.542 -86.653 -84.753 -82.851 -80.947 -79.043 -77.139 -75.235 -73.331 -71.426\n";
         print "-69.522 -67.617 -65.713 -63.808 -61.903 -59.999 -58.094 -56.189 -54.285 -52.380\n";
         print "-50.475 -48.571 -46.666 -44.761 -42.856 -40.952 -39.047 -37.142 -35.238 -33.333\n";
         print "-31.428 -29.523 -27.619 -25.714 -23.809 -21.904 -20.000 -18.095 -16.190 -14.286\n";
         print "-12.381 -10.476  -8.571  -6.667  -4.762  -2.857  -0.952   0.952   2.857   4.762\n";
         print "  6.667   8.571  10.476  12.381  14.286  16.190  18.095  20.000  21.904  23.809\n";
         print " 25.714  27.619  29.523  31.428  33.333  35.238  37.142  39.047  40.952  42.856\n";
         print " 44.761  46.666  48.571  50.475  52.380  54.285  56.189  58.094  59.999  61.903\n";
         print " 63.808  65.713  67.617  69.522  71.426  73.331  75.235  77.139  79.043  80.947\n";
         print " 82.851  84.753  86.653  88.542\n";
         }
	 elsif ($ny == 64) {
         print " -87.864 -85.097 -82.313 -79.526 -76.737 -73.948 -71.158 -68.368 -65.578 -62.787\n";
         print " -59.997 -57.207 -54.416 -51.626 -48.835 -46.045 -43.254 -40.464 -37.673 -34.883\n";
         print " -32.092 -29.301 -26.511 -23.720 -20.930 -18.139 -15.348 -12.558  -9.767  -6.977\n";
         print "  -4.186  -1.395   1.395   4.186   6.977   9.767  12.558  15.348  18.139  20.930\n";
         print "  23.720  26.511  29.301  32.092  34.883  37.673  40.464  43.254  46.045  48.835\n";
         print "  51.626  54.416  57.207  59.997  62.787  65.578  68.368  71.158  73.948  76.737\n";
         print "  79.526  82.313  85.097  87.864\n";
         }
         elsif ($ny == 160) {
         print "-89.142 -88.029 -86.911 -85.791 -84.670 -83.549 -82.428 -81.307 -80.185 -79.064\n";
         print "-77.943 -76.821 -75.700 -74.578 -73.457 -72.336 -71.214 -70.093 -68.971 -67.850\n";
         print "-66.728 -65.607 -64.485 -63.364 -62.242 -61.121 -60.000 -58.878 -57.757 -56.635\n";
         print "-55.514 -54.392 -53.271 -52.149 -51.028 -49.906 -48.785 -47.663 -46.542 -45.420\n";
         print "-44.299 -43.177 -42.056 -40.934 -39.813 -38.691 -37.570 -36.448 -35.327 -34.205\n";
         print "-33.084 -31.962 -30.841 -29.719 -28.598 -27.476 -26.355 -25.234 -24.112 -22.991\n";
         print "-21.869 -20.748 -19.626 -18.505 -17.383 -16.262 -15.140 -14.019 -12.897 -11.776\n";
         print "-10.654  -9.533  -8.411  -7.290  -6.168  -5.047  -3.925  -2.804  -1.682  -0.561\n";
         print "  0.561   1.682   2.804   3.925   5.047   6.168   7.290   8.411   9.533  10.654\n";
         print " 11.776  12.897  14.019  15.140  16.262  17.383  18.505  19.626  20.748  21.869\n";
         print " 22.991  24.112  25.234  26.355  27.476  28.598  29.719  30.841  31.962  33.084\n";
         print " 34.205  35.327  36.448  37.570  38.691  39.813  40.934  42.056  43.177  44.299\n";
         print " 45.420  46.542  47.663  48.785  49.906  51.028  52.149  53.271  54.392  55.514\n";
         print " 56.635  57.757  58.878  60.000  61.121  62.242  63.364  64.485  65.607  66.728\n";
         print " 67.850  68.971  70.093  71.214  72.336  73.457  74.578  75.700  76.821  77.943\n";
         print " 79.064  80.185  81.307  82.428  83.549  84.670  85.791  86.911  88.029  89.142\n";
         }
         elsif ($ny == 256) {
            print " -89.4628 -88.7670 -88.0670 -87.3661 -86.6648 -85.9634 -85.2618 -84.5603\n";
            print " -83.8586 -83.1570 -82.4553 -81.7536 -81.0519 -80.3502 -79.6485 -78.9468\n";
            print " -78.2451 -77.5434 -76.8416 -76.1399 -75.4382 -74.7364 -74.0347 -73.3330\n";
            print " -72.6312 -71.9295 -71.2277 -70.5260 -69.8243 -69.1225 -68.4208 -67.7190\n";
            print " -67.0173 -66.3155 -65.6138 -64.9120 -64.2103 -63.5086 -62.8068 -62.1051\n";
            print " -61.4033 -60.7016 -59.9998 -59.2981 -58.5963 -57.8946 -57.1928 -56.4911\n";
            print " -55.7893 -55.0876 -54.3858 -53.6841 -52.9823 -52.2806 -51.5788 -50.8771\n";
            print " -50.1753 -49.4736 -48.7718 -48.0701 -47.3683 -46.6666 -45.9648 -45.2630\n";
            print " -44.5613 -43.8595 -43.1578 -42.4560 -41.7543 -41.0525 -40.3508 -39.6490\n";
            print " -38.9473 -38.2455 -37.5438 -36.8420 -36.1403 -35.4385 -34.7368 -34.0350\n";
            print " -33.3333 -32.6315 -31.9298 -31.2280 -30.5263 -29.8245 -29.1227 -28.4210\n";
            print " -27.7192 -27.0175 -26.3157 -25.6140 -24.9122 -24.2105 -23.5087 -22.8070\n";
            print " -22.1052 -21.4035 -20.7017 -20.0000 -19.2982 -18.5965 -17.8947 -17.1929\n";
            print " -16.4912 -15.7894 -15.0877 -14.3859 -13.6842 -12.9824 -12.2807 -11.5789\n";
            print " -10.8772 -10.1754  -9.4737  -8.7719  -8.0702  -7.3684  -6.6667  -5.9649\n";
            print "  -5.2631  -4.5614  -3.8596  -3.1579  -2.4561  -1.7544  -1.0526  -0.3509\n";
            print "   0.3509   1.0526   1.7544   2.4561   3.1579   3.8596   4.5614   5.2631\n";
            print "   5.9649   6.6667   7.3684   8.0702   8.7719   9.4737  10.1754  10.8772\n";
            print "  11.5789  12.2807  12.9824  13.6842  14.3859  15.0877  15.7894  16.4912\n";
            print "  17.1929  17.8947  18.5965  19.2982  20.0000  20.7017  21.4035  22.1052\n";
            print "  22.8070  23.5087  24.2105  24.9122  25.6140  26.3157  27.0175  27.7192\n";
            print "  28.4210  29.1227  29.8245  30.5263  31.2280  31.9298  32.6315  33.3333\n";
            print "  34.0350  34.7368  35.4385  36.1403  36.8420  37.5438  38.2455  38.9473\n";
            print "  39.6490  40.3508  41.0525  41.7543  42.4560  43.1578  43.8595  44.5613\n";
            print "  45.2630  45.9648  46.6666  47.3683  48.0701  48.7718  49.4736  50.1753\n";
            print "  50.8771  51.5788  52.2806  52.9823  53.6841  54.3858  55.0876  55.7893\n";
            print "  56.4911  57.1928  57.8946  58.5963  59.2981  59.9998  60.7016  61.4033\n";
            print "  62.1051  62.8068  63.5086  64.2103  64.9120  65.6138  66.3155  67.0173\n";
            print "  67.7190  68.4208  69.1225  69.8243  70.5260  71.2277  71.9295  72.6312\n";
            print "  73.3330  74.0347  74.7364  75.4382  76.1399  76.8416  77.5434  78.2451\n";
            print "  78.9468  79.6485  80.3502  81.0519  81.7536  82.4553  83.1570  83.8586\n";
            print "  84.5603  85.2618  85.9634  86.6648  87.3661  88.0670  88.7670  89.4628\n";
         }
         else {
             print STDERR "*** script needs to be modified ***\n";
             print STDERR "unknown gaussian grid-defined grid\n";
         }
         last GRD; };

      print STDERR "*** script needs to be modified ***\n";
      print STDERR "unknown user-defined grid\n";
   }
}


# make the tdef statement

&tdef;

# ------------------var-------------------------------------;

%tails =(
   '1' => 'sfc',
   '2' => 'clb',
   '3' => 'clt',
   '4' => 'zdg',
   '5' => 'lcl',
   '6' => 'mwl',
   '7' => 'trp',
   '8' => 'toa',
   '9' => 'bos',
   '10' => 'clm',
   '12' => 'lcb',
   '13' => 'lct',
   '14' => 'loc',
   '22' => 'mcb',
   '23' => 'mct',
   '24' => 'mdc',
   '32' => 'hcb',
   '33' => 'hct',
   '34' => 'hic',
   '100' => 'prs',
   '101' => 'plr',
   '102' => 'msl',
   '103' => 'hml',
   '104' => 'zlr',
   '105' => 'hag',
   '106' => 'hlr',
   '107' => 'sig',
   '108' => 'slr',
   '109' => 'hbl',
   '110' => 'blr',
   '111' => 'dpl',
   '112' => 'dlr',
   '113' => 'tht',
   '114' => 'tlr',
   '116' => 'plg',
   '121' => 'plr',
   '128' => 'slr',
   '141' => 'plr',
   '160' => 'dsl',
   '200' => 'clm',
   '204' => 'htfl',
   '209' => 'bcb',
   '210' => 'bct',
   '211' => 'bcl',
   '212' => 'lcb',
   '213' => 'lct',
   '214' => 'lcl',
   '222' => 'mcb',
   '223' => 'mct',
   '224' => 'mcl',
   '232' => 'hcb',
   '233' => 'hct',
   '234' => 'hcl',
   '242' => 'cvb',
   '243' => 'cvt',
   '244' => 'cvc',
   );
$tails{'100'} = "$prs";

$nlevelmax=0;
$levelsmax=0;

$nvar=0;
foreach $fname (sort keys(%flevels)) {
   ($name, $kpds6) = split(/:/, $fname);
   ($kpds5, $comment) = split(/:/, $fcomments{$fname});
   $comment = substr($comment,1);

   #
   # find number of levels
   #

   $_=$flevels{$fname};
   $nlev = (tr/ / /) - 1;
   $kpds7s = $_;

   # fix names to be grads compatible
   # eliminate dashes, underscores, blanks and put no in front of leading digits

   $_ = $name;
   $_ =~ tr/_\- //d;
   if ( /^[0-9]/ ) { $_ = "no$_"; }
   $name = $_;
   
   $tail = $suffix eq 'no' ? "" : $tails{$kpds6};

   # tranlate special levels

   if ($kpds6 == 1) {
      $var_line[$nvar++]="${name}sfc  0 $kpds5,$kpds6,0 ** surface $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 6) {
      $var_line[$nvar++]="${name}mwl  0 $kpds5,$kpds6,0 ** max wind level $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 7) {
      $var_line[$nvar++]="${name}trp  0 $kpds5,$kpds6,0 ** tropopause $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 8) {
      $var_line[$nvar++]="${name}toa  0 $kpds5,$kpds6,0 ** top of atmos $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 200) {
      $var_line[$nvar++]="${name}clm  0 $kpds5,$kpds6,0 ** atmos column $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 204) {
      $var_line[$nvar++]="${name}htfl  0 $kpds5,$kpds6,0 ** highest trop freezing level $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 212) {
      $var_line[$nvar++]="${name}lcb  0 $kpds5,$kpds6,0 ** low cloud base $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 213) {
      $var_line[$nvar++]="${name}lct  0 $kpds5,$kpds6,0 ** low cloud top $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 214) {
      $var_line[$nvar++]="${name}lcl  0 $kpds5,$kpds6,0 ** low cloud level $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 222) {
      $var_line[$nvar++]="${name}mcb  0 $kpds5,$kpds6,0 ** mid cloud base $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 223) {
      $var_line[$nvar++]="${name}mct  0 $kpds5,$kpds6,0 ** mid cloud top $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 224) {
      $var_line[$nvar++]="${name}mcl  0 $kpds5,$kpds6,0 ** mid cloud level $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 232) {
      $var_line[$nvar++]="${name}hcb  0 $kpds5,$kpds6,0 ** high cloud base $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 233) {
      $var_line[$nvar++]="${name}hct  0 $kpds5,$kpds6,0 ** high cloud top $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 234) {
      $var_line[$nvar++]="${name}hcl  0 $kpds5,$kpds6,0 ** high cloud level $comment";
      $nlev=0;
   }

   if ($kpds6 == 103 && ($kpds7s =~ s/ 1829 / /)) {
      $var_line[$nvar++]="${name}1829m  0 $kpds5,$kpds6,1829 ** 1829 m $comment";
      $nlev--;
   }
   if ($kpds6 == 103 && ($kpds7s =~ s/ 2743 / /)) {
      $var_line[$nvar++]="${name}2743m  0 $kpds5,$kpds6,2743 ** 2743 m $comment";
      $nlev--;
   }
   if ($kpds6 == 103 && ($kpds7s =~ s/ 3658 / /)) {
      $var_line[$nvar++]="${name}3658m  0 $kpds5,$kpds6,3658 ** 3658 m $comment";
      $nlev--;
   }
   if ($kpds6 == 105 && ($kpds7s =~ s/ 2 / /)) {
      $var_line[$nvar++]="${name}2m  0 $kpds5,$kpds6,2 ** 2 m $comment";
      $nlev--;
   }
   if ($kpds6 == 105 && ($kpds7s =~ s/ 10 / /)) {
      $var_line[$nvar++]="${name}10m  0 $kpds5,$kpds6,10 ** 10 m $comment";
      $nlev--;
   }
   if ($kpds6 == 107 && ($kpds7s =~ s/ 9950 / /) && $nlev < 5) {
      $var_line[$nvar++]="${name}sig995  0 $kpds5,$kpds6,9950 ** sig=.995 $comment";
      $nlev--;
   }
   if ($kpds6 == 116 && ($kpds7s =~ s/ 7680 / /)) {
      $var_line[$nvar++]="${name}30_0mb  0 $kpds5,$kpds6,7680 ** 30-0 mb above gnd $comment";
      $nlev--;
   }
   if ($kpds6 == 116 && ($kpds7s =~ s/ 15390 / /)) {
      $var_line[$nvar++]="${name}60_30mb  0 $kpds5,$kpds6,15390 ** 60-30 mb above gnd $comment";
      $nlev--;
   }
   if ($kpds6 == 116 && ($kpds7s =~ s/ 23100 / /)) {
      $var_line[$nvar++]="${name}90_60mb  0 $kpds5,$kpds6,23100 ** 90-60 mb above gnd $comment";
      $nlev--;
   }
   if ($kpds6 == 116 && ($kpds7s =~ s/ 30810 / /)) {
      $var_line[$nvar++]="${name}120_90mb  0 $kpds5,$kpds6,30810 ** 90-60 mb above gnd $comment";
      $nlev--;
   }
   if ($kpds6 == 116 && ($kpds7s =~ s/ 38520 / /)) {
      $var_line[$nvar++]="${name}150_120mb  0 $kpds5,$kpds6,38520 ** 150-120 mb above gnd $comment";
      $nlev--;
   }
   if ($kpds6 == 116 && ($kpds7s =~ s/ 46080 / /)) {
      $var_line[$nvar++]="${name}180_0mb  0 $kpds5,$kpds6,46080 ** 180-0 mb above gnd $comment";
      $nlev--;
   }
   if ($kpds6 == 116 && ($kpds7s =~ s/ 46230 / /)) {
      $var_line[$nvar++]="${name}180_150mb  0 $kpds5,$kpds6,46230 ** 180-150 mb above gnd $comment";
      $nlev--;
   }

   if ($model eq "ETA") {
      if ($kpds6 == 109 && ($kpds7s =~ s/ 1 / /)) {
         $var_line[$nvar++]="${name}hlev1  0 $kpds5,$kpds6,1 ** hybrid level 1 $comment";
         $nlev--;
      }
      if ($kpds6 == 112 && ($kpds7s =~ s/ 10 / /)) {
         $var_line[$nvar++]="${name}0_10cm  0 $kpds5,$kpds6,10 ** 0-10 cm undergnd $comment";
         $nlev--;
      }
      if ($kpds6 == 112 && ($kpds7s =~ s/ 2600 / /)) {
         $var_line[$nvar++]="${name}10_40cm  0 $kpds5,$kpds6,2600 ** 10-40 cm undergnd $comment";
         $nlev--;
      }
      if ($kpds6 == 112 && ($kpds7s =~ s/ 10340 / /)) {
         $var_line[$nvar++]="${name}40_100cm  0 $kpds5,$kpds6,10340 ** 40-100 cm undergnd $comment";
         $nlev--;
      }
      if ($kpds6 == 112 && ($kpds7s =~ s/ 25800 / /)) {
         $var_line[$nvar++]="${name}100_200cm  0 $kpds5,$kpds6,25800 ** 100-200 cm undergnd $comment";
         $nlev--;
      }
   }
   else {
      if ($kpds6 == 111 && ($kpds7s =~ s/ 300 / /)) {
         $var_line[$nvar++]="${name}SoilB  0 $kpds5,$kpds6,300 ** $comment";
         $nlev--;
      }
      if ($kpds6 == 112 && ($kpds7s =~ s/ 10 / /)) {
         $var_line[$nvar++]="${name}SoilT  0 $kpds5,$kpds6,10 ** 0-10cm undergnd $comment";
         $nlev--;
      }
      if ($kpds6 == 112 && ($kpds7s =~ s/ 2760 / /)) {
         $var_line[$nvar++]="${name}SoilM  0 $kpds5,$kpds6,2760 ** 10-200 cm undergnd $comment";
         $nlev--;
      }
   }

   if ($nlev == 1) {
      $kpds7s =~ s/^ //;
      $var_line[$nvar++]="$name$tail  0 $kpds5,$kpds6,$kpds7s ** $comment";
   }
   elsif ($nlev > 1) {
      $var_line[$nvar++]="$name$tail $nlev $kpds5,$kpds6,0 ** $comment";
      if ($nlev > $nlevelmax) {
         $nlevelmax=$nlev;
         $levelsmax=$flevels{$fname};
      }
   }
}

#------------------levels-------------------------;

if ($nlevelmax == 0) {
   print "zdef 1 linear 1 1\n";
}
else {

   ($_ = $levelsmax) =~ s/.//;
   chop($_);

   if ($z_order eq "theta") {
      @levels=sort {$a <=> $b} split(/ /,$_);
   }
   else {
      @levels=sort {$b <=> $a} split(/ /,$_);
   }

   print "zdef $nlevelmax levels\n";
   for ($i = 0; $i < $nlevelmax; $i++) {
      print "$levels[$i] ";
   }
   print "\n";
}

print "vars $nvar\n";
for ($i = 0; $i < $nvar; $i++) {
   print $var_line[$i];
}
print "ENDVARS\n";

if ($sys eq "win") {
   unlink $TmpFile;
}
unlink $ListA;
exit 0;

#------------------ jday --------------------
# jday(year,mo,day) return the julian day relative to jan 0
# mo=1..12
#
sub jday {

   local($n);
   $n=substr(" 000 031 059 090 120 151 181 212 243 273 304 334",($_[1]-1)*4,4);
   $n = $n + $_[2];

   if ($_[1] > 2 && $_[0] % 4 == 0) {
      if ($_[0] % 400 == 0 || $_[0] % 100 != 0) {
         $n++;
      }
   }
   $n;
}


#------------------ write tdef statement ------------------
# still not great but better than before

sub tdef {

   local($tmp);
   $dt="1mo";
   if ($ntime == 1) {
      $dt="1mo";
   }
   elsif ($hour != $hour1) {
      # assume that dt < 24 hours
      $tmp=$hour1-$hour;
      ($tmp < 0) && do {$tmp = $tmp + 24};
      $dt="${tmp}hr";
   }
   elsif ($day != $day1) {
      # assume that dt < 365 days
      $tmp = &jday($year1,$mo1,$day1) - &jday($year,$mo,$day);
      ($tmp < 0) && do {$tmp = $tmp + &jday($year,12,31)};
      $dt="${tmp}dy";
   }
   elsif ($mo != $mo1) {
      # assume that dt < 12 months
      $tmp = $mo1 - $mo;
      ($tmp < 0) && do {$tmp = $tmp + 12};
      $dt="${tmp}mo";
   }
   else {
      $tmp = $year1 - $year;
      $dt="${tmp}yr";
   }
   print "tdef $ntime linear ${hour}Z$day$month$year $dt\n";
}
