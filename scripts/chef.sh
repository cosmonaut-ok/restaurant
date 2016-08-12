#!/bin/bash

. config.in

CHEF_URL="https://raw.githubusercontent.com/mpasternacki/chef-mode/master/chef-mode.el"
KITCHEN_URL="https://raw.githubusercontent.com/cosmonaut-ok/test-kitchen-el/master/test-kitchen.el"
KNIFE_URL="https://raw.githubusercontent.com/bryanwb/knife-mode/master/knife.el"

function pkg_install
{
    _PWD=`pwd`
    cd $TMP
    get_url_with_name chef-mode.el $CHEF_URL chef-mode.el
    get_url_with_name test-kitchen.el $KITCHEN_URL test-kitchen.el
    get_url_with_name knife.el $KNIFE_URL knife.el
    mkdir $DST/chef-mode
    cp chef-mode.el $DST/chef-mode
    cp test-kitchen.el $DST/chef-mode
    cp knife.el $DST/chef-mode
    cd $_PWD
    $RM -rf $TMP
}


function pkg_update
{
    :
}

. include.in
