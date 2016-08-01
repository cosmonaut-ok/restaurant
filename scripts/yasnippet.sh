#!/bin/bash

. config.in

YAS_COMPANY_URL="https://raw.githubusercontent.com/company-mode/company-mode/master/company-yasnippet.el"

function pkg_install
{
    _PWD=`pwd`
    cd $TMP
    get_url_with_name company-yasnippet.el $YAS_COMPANY_URL company-yasnippet.el
    mkdir $DST/company-yasnippet
    cp company-yasnippet.el $DST/company-yasnippet
    cd $_PWD
    $RM -rf $TMP
}


function pkg_update
{
    :
}

. include.in
