#!/bin/bash

. config.in

HSVIS_URL="https://www.emacswiki.org/emacs/download/hideshowvis.el"
HL_LINE_PLUS_URL="https://www.emacswiki.org/emacs/download/hl-line%2b.el"
REDO_URL="https://www.emacswiki.org/emacs/download/redo%2b.el"
GH_NOTIFIER_URL="https://raw.githubusercontent.com/xuchunyang/github-notifier.el/master/github-notifier.el"

function pkg_install
{
    _PWD=`pwd`
    cd $TMP
    get_url_with_name hideshowvis.el $HSVIS_URL hideshowvis.el
    get_url_with_name hl-line-plus.el $HL_LINE_PLUS_URL hl-line-plus.el
    get_url_with_name redo+.el $REDO_URL redo+.el
    get_url_with_name github-notifier.el $GH_NOTIFIER_URL github-notifier.el
    mkdir $DST/hideshowvis
    mkdir $DST/hl-line-plus
    mkdir $DST/redo+
    mkdir $DST/github-notifier
    cp hideshowvis.el $DST/hideshowvis
    cp hl-line-plus.el $DST/hl-line-plus/hl-line-plus.el
    cp redo+.el $DST/redo+/redo+.el
    cp github-notifier.el $DST/github-notifier/github-notifier.el
    cd $_PWD
    $RM -rf $TMP
}


function pkg_update
{
    :
}

. include.in
