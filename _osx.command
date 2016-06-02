#!/bin/sh

# change to working directory to location of command file: http://hints.macworld.com/article.php?story=20041217111834902
here="`dirname \"$0\"`"
cd "$here" || exit 1

#compile Surfice as 64-bit Cocoa
##lazbuild ./plyview.lpr --cpu=x86_64 --ws=cocoa --compiler="/usr/local/bin/ppcx64"
lazbuild ./plyview.lpr
strip  ./plyview

rm *.~*
rm  DS_STORE
rm *.dsm
rm *.bak
rm -rf lib
rm -rf backup

# cd /Users/rorden/Documents/pas/
# zip -FSr /Users/rorden/Documents/pas/ply.zip plyview
