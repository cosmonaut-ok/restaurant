#!/bin/bash

. config.in

RSPEC_MODE_URL="https://github.com/pezra/rspec-mode/archive/master.zip"
ERUBY_URL="https://raw.githubusercontent.com/petere/emacs-eruby-mode/master/eruby-mode.el"

function pkg_install
{
    _PWD=`pwd`
    cd $TMP
    get_url_with_name rspec-mode.zip $RSPEC_MODE_URL rspec-mode.zip
    get_url_with_name eruby-mode.el $ERUBY_URL eruby-mode.el
    mkdir $DST/ruby-mode-extensions $DST/rspec-mode
    cp eruby-mode.el $DST/ruby-mode-extensions
    unzip rspec-mode.zip
    cp -r rspec-mode-master/* $DST/rspec-mode
    cd $_PWD
    $RM -rf $TMP
}


function pkg_update
{
    :
}

. include.in
