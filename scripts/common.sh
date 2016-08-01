#!/bin/bash

. config.in

HSVIS_URL="https://www.emacswiki.org/emacs/download/hideshowvis.el"
HL_LINE_PLUS_URL="https://www.emacswiki.org/emacs/download/hl-line%2b.el"
REDO_URL="www.wonderworks.com/download/redo.el"

function pkg_install
{
    _PWD=`pwd`
    cd $TMP
    get_url_with_name hideshowvis.el $HSVIS_URL hideshowvis.el
    get_url_with_name hl-line-plus.el $HL_LINE_PLUS_URL hl-line-plus.el
    get_url_with_name redo.el $REDO_URL redo.el
    mkdir $DST/hideshowvis
    mkdir $DST/hl-line-plus
    mkdir $DST/redo
    cp hideshowvis.el $DST/hideshowvis
    cp hl-line-plus.el $DST/hl-line-plus
    cp redo.el $DST/redo
    cd $_PWD
    $RM -rf $TMP
}


function pkg_update
{
    :
}

. include.in
