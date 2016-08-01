all: build install

build:
	@echo build

install:
	@echo install

# #!/bin/bash

# set -e

# realpath () {
#     local r=$1; local t=$(readlink $r)
#     while [ $t ]; do
#         r=$(cd $(dirname $r) && cd $(dirname $t) && pwd -P)/$(basename $t)
#         t=$(readlink $r)
#     done
#     echo $r
# }

# [ ! -z $EMACS ] || EMACS=`which emacs`

# REQUIRED_APPS="convert ruby git"
# BIN_DIR=~/bin/
# ME=$(dirname $(realpath $0))

# if [ ! -x "$EMACS" ] && [ ! -x "$(which $EMACS)" ]; then
#     echo "You must install emacs (v. 24 or higher) before running script"
#     exit 1
# fi

# for i in $REQUIRED_APPS; do
#     if [ -z `which $i 2>/dev/null` ]; then
#         echo "You must install realpath, imagemagik, ruby and git before running this script"
#         exit 1
#     fi
# done

# if [ -z "$(which git)" ]; then
#     echo "Install ``git'' before bootstrapping"
#     exit 1
# elif [ -z "$(which hg)" ]; then
#     echo "Install ``mercurial'' before bootstrapping"
#     exit 1
# elif [ -z "$(which autoconf)" ]; then
#     echo "Install ``autoconf'' before bootstrapping"
#     exit 1
# elif [ -z "$(which automake)" ]; then
#     echo "Install ``automake'' before bootstrapping"
#     exit 1
# fi

# do_build()
# {
#     cd ${ME}
#     $EMACS -Q --debug-init --script "$(dirname $0)"/bootstrap.el
#     cd ${ME}/scripts/
#     ./build_all
# }

# install_desktop_and_icons()
# {
#     echo -n "Installing .desktop files and icons to $1"
#     cd ${ME}/data
#     for i in 128x128 16x16 20x20 24x24 256x256 32x32 48x48 512x512 64x64 96x96; do
#         convert -resize ${i} restaurant.svg restaurant_${i}.png
#         mkdir -p "$1"/share/icons/hicolor/"$i"/apps/
#         mv ${ME}/data/restaurant_"$i".png "$1"/share/icons/hicolor/${i}/apps/restaurant.png
#         echo -n '.'
#     done
#     mkdir -p ${1}/share/applications/
#     cp restaurant.desktop "$1"/share/applications/
#     echo '[ DONE ]'
# }

# do_install()
# {
#     if [ -z $1 ]; then
#         local dst='--local'
#     else
#         dst=$1
#     fi
#     if [ $dst == '--global' ]; then
#         if [ ! $UID == 0 ]; then
#             echo "Installation option \`\`--global\`\` used, but user is not root. Exiting"
#             exit 1
#         fi
#         local install_prefix="/usr/"
#         local local_install=0
#     else
#         if [ $UID == 0 ]; then
#             echo "Installation option \`\`--global\`\` not used, but user is root. Exiting"
#             exit 1
#         fi
#         local install_prefix=~/.local/
#         local local_install=1
#     fi
#     cd ${ME}
#     # making bin directory
#     [ -d "$install_prefix" ] || mkdir -p "$install_prefix"
#     # copy binary
#     mkdir -p "$install_prefix"/bin/
#     cp -f $ME/bin/restaurant "$install_prefix"/bin/ && chmod a+x "$install_prefix"/bin/restaurant
#     #
#     if [ $local_install -eq 1 -a -z "$(grep -E 'PATH=.*:~/bin' ~/.profile | grep -vE '^#')" -a -z "$(grep -E 'PATH=.*:~/bin' ~/.bashrc | grep -vE '^#')" ]; then
#         echo export PATH=\"\$PATH:"$install_prefix"/bin\" >> ~/.profile
#         echo 'Now, you should make ``source ~/.profile``'
#     fi
#     ## install icon and .desktop file
#     install_desktop_and_icons "$install_prefix"
# }

# do_clean()
# {
#     cd ${ME}
#     echo -n "Clearing source directory..."
#     rm -rf share el-get elpa .packages.installed.p
#     find ${ME} -type f -name '*~' -delete
#     echo '[ DONE ]'
# }

# do_help()
# {
#     echo "USAGE $ME <COMMAND> [OPTIONS]"
#     echo "commands:"
#     echo "         help"
#     echo "         build"
#     echo "         install --global/--local"
#     echo "         clean"

# }

# case $1 in
#     "install")
#         shift
#         do_build $@
#         do_install $@
#         ;;
#     "clean")
#         do_clean
#         ;;
#     "build")
#         shift
#         do_build $@
#         ;;
#     *)
#         do_help
#         ;;
# esac
