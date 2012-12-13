#!/bin/sh
printenv
#Project root dir
XCODE_BASE=${PWD}

#Get list of source codes
FILES=`find "${XCODE_BASE}" -name "*.m" -o -name "*.h"`

for FILE in $FILES 
do
#Format each source
/usr/local/bin/uncrustify -l oc+ -c $HOME/uncrustify/obj-c.cfg --replace $FILE
echo Uncrustified $FILE
done
