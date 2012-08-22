#!/opt/local/bin/python
#-*-coding: utf-8 -*-

import datetime

deadline=datetime.datetime.strptime("2012-02-14 17:00:00","%Y-%m-%d %H:%M:%S")
now=datetime.datetime.now()
diff=deadline-now

hours=diff.days*24+diff.seconds/3600
minutes=(diff.seconds%3600)/60
seconds=diff.seconds%60

print u"Remaining: %s:%s:%s" % (hours,minutes,seconds)
