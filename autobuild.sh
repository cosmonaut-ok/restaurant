#!/bin/sh

autoconf
./configure --with-gui=gtk3
make release
make clean-emacs
