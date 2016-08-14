#!/bin/bash

. config.in

RSPEC_MODE_URL="https://github.com/pezra/rspec-mode/archive/master.zip"
ERUBY_URL="https://raw.githubusercontent.com/cosmonaut-ok/emacs-eruby-mode/master/eruby-mode.el"
BUNDLER_URL="https://raw.githubusercontent.com/tobiassvn/bundler.el/master/bundler.el"

function pkg_install
{
    _PWD=`pwd`
    cd $TMP
    get_url_with_name rspec-mode.zip $RSPEC_MODE_URL rspec-mode.zip
    get_url_with_name eruby-mode.el $ERUBY_URL eruby-mode.el
    get_url_with_name bundler.el $BUNDLER_URL bundler.el
    mkdir $DST/eruby-mode $DST/rspec-mode
    cp eruby-mode.el $DST/eruby-mode
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
