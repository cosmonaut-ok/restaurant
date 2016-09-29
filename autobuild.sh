#!/bin/sh

autoconf
for i in gtk2 gtk3; do
    ./configure --with-gui=${i}
    make release
    make clean-emacs
done    
