#!/usr/bin/env python
import os
import sys
import commands

def simlink(src,dest):
    if cmd('ln -Fis ' + src + ' '+dest)==0:
        print 'Failed to link ' + src

def cmd(command):
    commands.getstatusoutput(command)[0]

#run
cmd('git clone https://github.com/erikw/tmux-powerline.git .tmux.d/tmux-powerline')
# http://d.hatena.ne.jp/Tetsujin/20120815/1345033377
cmd('wget -P .zsh.d/functions/ https://raw.github.com/tetsujin/zsh-function-mysql/master/mysql')

for dotfile in os.listdir('.'):
        simlink(os.getcwd() + '/' + dotfile, '~/' + dotfile)
