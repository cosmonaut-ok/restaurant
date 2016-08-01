#!/bin/bash

. config.in


GITHUB_CLONE_URL="https://raw.githubusercontent.com/dgtized/github-clone.el/master/github-clone.el"
YAGIST_URL="https://raw.githubusercontent.com/mhayashi1120/yagist.el/master/yagist.el"
MAGIT_GH_PULLS="https://raw.githubusercontent.com/sigma/magit-gh-pulls/master/magit-gh-pulls.el"
GITHUB_BROWSE_FILE="https://raw.githubusercontent.com/osener/github-browse-file/master/github-browse-file.el"

function pkg_install
{
    _PWD=`pwd`
    cd $TMP
    get_url_with_name github-clone.el $GITHUB_CLONE_URL github-clone.el
    get_url_with_name yagist.el $YAGIST_URL yagist.el
    get_url_with_name magit-gh-pulls.el $MAGIT_GH_PULLS magit-gh-pulls.el
    get_url_with_name github-browse-file.el $GITHUB_BROWSE_FILE github-browse-file.el
    
    mkdir $DST/github-clone
    mkdir $DST/yagist
    mkdir $DST/magit-gh-pulls
    mkdir $DST/github-browse-file
    cp github-clone.el $DST/github-clone
    cp yagist.el $DST/yagist
    cp magit-gh-pulls.el $DST/magit-gh-pulls
    cp github-browse-file.el $DST/github-browse-file

    cd $_PWD
    $RM -rf $TMP
}


function pkg_update
{
    :
}

. include.in
