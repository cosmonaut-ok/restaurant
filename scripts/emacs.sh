#!/bin/bash

EMACS_URL="http://ftp.gnu.org/pub/gnu/emacs/emacs-24.5.tar.gz"

_PWD=$PWD

MYDIR=`mktemp -d /tmp/emacs-tmp.XXXX`

find_projdir() {
    
}





echo $MYDIR

cd $MYDIR

curl -s $EMACS_URL > $(basename $EMACS_URL)
tar -xf $(basename $EMACS_URL)
cd $(basename $EMACS_URL .tar.gz)

mkdir $MYDIR/emacs-build
./configure --prefix=$MYDIR/emacs-build --without-pop --with-x-toolkit=gtk

make install DESTDIR $MYDIR/emacs-build
