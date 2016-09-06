#!/bin/sh

autoconf
./configure
make release
make clean-emacs
./configure --with-gui=gtk2
make release
