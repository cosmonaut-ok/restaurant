#!/bin/bash

. config.in

PAREDIT_URL="http://mumble.net/~campbell/emacs/paredit.el"

function pkg_install
{
    PWD=`pwd`
    cd $TMP
    get_url_with_name paredit.el $PAREDIT_URL paredit.el
    mkdir -p $DST/paredit
    cp paredit.el $DST/paredit
    cd $PWD
    $RM -rf $TMP
}


function pkg_update
{
    :
}

. include.in
