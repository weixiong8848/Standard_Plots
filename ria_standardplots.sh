#! /bin/bash

#if [ $# -ne 7 ]
#then
#  echo "Usage: `basename $0` {dssat input} {dssat output}"
#  exit -1
#fi
ttype=$1
variable=$2
confinput=$3
pngoutput=$4

command -V R >/dev/null 2>&1 || { echo >&2 " 'R' is required by this tool was not found on past";exit 1;}

INSTALL_DIR=/mnt/galaxyTools/ria_standardplots/1.0.0
ria_standardplots=$INSTALL_DIR/ria_standardplots.r

xvfb-run R --no-save --vanilla --slave --args $ttype $variable $confinput $pngoutput< $ria_standardplot

exit
