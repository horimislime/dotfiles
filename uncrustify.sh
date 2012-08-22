#!/bin/sh

echo -n "%%%{PBXSelection}%%%"
/usr/local/bin/uncrustify -q -l oc+ -c /Users/horimi_soichiro/uncrustify/obj-c.cfg <&0
echo -n "%%%{PBXSelection}%%%"