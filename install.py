#!/usr/bin/env python
import os
import sys
import commands

def simlink(src,dest):
    if cmd('ln -Fis ' + src + ' '+dest)==0:
        print 'Failed to link ' + src

def cmd(command):
    commands.getstatusoutput(command)[0]

for dotfile in os.listdir('.'):
        simlink(os.getcwd() + '/' + dotfile, '~/' + dotfile)
