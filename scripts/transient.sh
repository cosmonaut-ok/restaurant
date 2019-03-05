#!/bin/bash

. config.in

TRANSIENT_URL="https://raw.githubusercontent.com/magit/transient/master/lisp/transient.el"

function pkg_install
{
    get_url_with_name transient.el $TRANSIENT_URL
    copy_to_local transient.el transient
}


function pkg_update
{
    :
}

. include.in
