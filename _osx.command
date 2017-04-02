#!/bin/sh

# change to working directory to location of command file: http://hints.macworld.com/article.php?story=20041217111834902
here="`dirname \"$0\"`"
cd "$here" || exit 1

#compile to 64-bit Cocoa (Carbon does not support modern OpenGL)
##lazbuild ./plyview.lpr --cpu=x86_64 --ws=cocoa --compiler="/usr/local/bin/ppcx64"
#compile to Cocoa (Carbon does not support modern OpenGL)
lazbuild ./plyview.lpr --ws=cocoa
strip  ./plyview

rm *.~*
rm  DS_STORE
rm *.dsm
rm *.bak
rm -rf lib
rm -rf backup

# cd /Users/rorden/Documents/pas/
# zip -FSr /Users/rorden/Documents/pas/ply.zip plyview
