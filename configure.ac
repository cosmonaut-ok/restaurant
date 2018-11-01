# configure.ac --- autoconfiguration file  -*- Autoconf -*-

# Copyright (C) 2016 Alexander aka 'CosmonauT' Vynnyk

# Maintainer: cosmonaut.ok@zoho.com
# Keywords: internal
# Package: restaurant

# This file is part of Restaurant.

# Restaurant is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# Restaurant is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with Restaurant.  If not, see <http://www.gnu.org/licenses/>.

# Commentary:

# TODO:

# Code:

AC_PREREQ(2.69)

m4_define([M4_RESTAURANT_VERSION], m4_esyscmd(grep autoconf-anchor src/restaurant-help.el | cut -d\" -f2 | tr -d \\n))

AC_INIT([Restaurant Chef IDE], [M4_RESTAURANT_VERSION], cosmonaut.ok@zoho.com)

AC_SUBST(RESTAURANT_VERSION, M4_RESTAURANT_VERSION)
AC_SUBST(EMACS_URI, https://github.com/emacs-mirror/emacs/archive/emacs-26.1.tar.gz)
AC_SUBST(EMACS_RESTAURANT_URI, https://sourceforge.net/projects/restaurant-ide/files/static/emacs-restaurant.tar.gz/download)
AC_SUBST(ELGET_RESTAURANT_URI, https://sourceforge.net/projects/restaurant-ide/files/static/el-get-packages.tar.gz/download)
AC_SUBST(EMACS_CFLAGS)
AC_SUBST(BUILD_DIR, /tmp/restaurant-release)
AC_SUBST(GUI_TOOLKIT)
AC_SUBST(LOCAL_EMACS_DIR, emacs)
AC_SUBST(RELEASE_FILES, ["restaurant data LICENSE src README.md el-get $LOCAL_EMACS_DIR lib Gemfile bootstrap.sh etc"])
AC_SUBST(EMACS)
AC_SUBST(BUILD_OPTIONS, ["--without-pop --with-gnutls"])
AC_SUBST(BYTE_COMPILE_P)
AC_SUBST(STATIC)
AC_SUBST(EMACS_NAME, restaurant-bin)

# Checks for programs.
AC_ARG_WITH(gui,      [  --with-gui=TOOLKIT      use gtk2, gtk3, motif, athena, no (default gtk3)], GUI_TOOLKIT=$withval, GUI_TOOLKIT="gtk3")
AC_ARG_WITH(emacs,    [  --with-emacs=path       use the custom path to emacs binary], EMACS=$withval, EMACS=$EMACS)
AC_ARG_WITH(gif,      [  --with-gif              build emacs with/without gif support (default no)], GIF=$withval, GIF="no")
AC_ARG_WITH(png,      [  --with-png              build emacs with/without png support (default no)], PNG=$withval, PNG="no")
AC_ARG_WITH(jpeg,     [  --with-jpeg             build emacs with/without jped support (default no)], JPEG=$withval, JPEG="no")
AC_ARG_WITH(tiff,     [  --with-tiff             build emacs with/without tiff support (default no)], TIFF=$withval, TIFF="no")
AC_ARG_WITH(cflags,   [  --with-emacs-cflags     build emacs with specific emacs cflags], EMACS_CFLAGS=$withval, EMACS_CFLAGS="-no-pie")
AC_ARG_WITH(makeinfo, [  --with-makeinfo         build emacs with/without makeinfo additional information (default no)], MAKEINFO=$withval, MAKEINFO="no")
# AC_ARG_WITH(xml2,     [  --without-xml2             build emacs with/without xml2 support (default no)], ECL_PATH=$withval, XML2="auto")
AC_ARG_ENABLE(byte-compile, [  --enable-byte-compile  Enables Restaurant bute compilation (experimental, default: disabled)], BYTE_COMPILE_P=$enableval, BYTE_COMPILE_P=no)
AC_ARG_ENABLE(static, [  --disable-static  Enables static precompiled archives, instead of full build, default: enabled], STATIC=$enableval, STATIC=yes)

# check for makeinfo
AC_CHECK_PROG(GIT,git,yes,no)
AC_CHECK_PROG(CURL,curl,yes,no)
AC_CHECK_PROG(MERCURIAL,hg,yes,no)
AC_CHECK_PROG(RUBY,ruby,yes,no)
AC_CHECK_PROG(BUNDLE,bundle,yes,no)
AC_CHECK_PROG(MARKDOWN,markdown,yes,no)
AC_CHECK_PROG(RSVG_CONVERT,rsvg-convert,yes,no)
AC_CHECK_PROG(INKSCAPE,inkscape,yes,no)
AC_CHECK_PROG(IMAGEMAGICK,convert,yes,no)
# check for lib deps
AC_CHECK_LIB(gnutls , gnutls_global_deinit, , [AC_MSG_ERROR([Couldn't find gnutls library])] )
# check for build deps
AC_CHECK_HEADER([X11/Xlib-xcb.h])
AC_CHECK_HEADER([X11/xpm.h])

if test $CURL == "no"; then
   AC_MSG_ERROR([there is no curl package in system. This binary required for correct restaurant build (build only)])
fi

if test $GIT == "no"; then
   AC_MSG_ERROR([there is no git package in system. This binary required for correct restaurant working])
fi

if test $MERCURIAL == "no"; then
   AC_MSG_ERROR([there is no mercurial package (hg) in system])
fi

if test $RUBY == "no"; then
   AC_MSG_WARN([there is no ruby package in system. You can not use restaurant (build only)])
fi

if test $BUNDLE == "no"; then
   AC_MSG_WARN([there is no bundler package in system. You can not use restaurant (build only)])
fi

if test $MARKDOWN == "no"; then
   AC_MSG_ERROR([there is no markdown package in system. This binary required for correct restaurant working])
fi

if test $RSVG_CONVERT == "no"; then
   AC_MSG_ERROR([there is no librsvg-bin package in system. Binary rsvg-convert required for correct restaurant working])
fi

if test $INKSCAPE == "no"; then
   AC_MSG_ERROR([there is no inkscape package in system. Binary inkscape required to convert svg to png images on-the-fly (required for build)])
fi

if test $IMAGEMAGICK == "no"; then
   AC_MSG_ERROR([there is no imagemagick package in system. Binary convert required to convert svg to png images on-the-fly (required for build)])
fi

if test x$GIF == "xno"; then
   BUILD_OPTIONS="$BUILD_OPTIONS --without-gif"
fi

if test x$PNG == "xno"; then
   BUILD_OPTIONS="$BUILD_OPTIONS --without-png"
fi

if test x$JPEG == "xno"; then
   BUILD_OPTIONS="$BUILD_OPTIONS --without-jpeg"
fi

if test x$TIFF == "xno"; then
   BUILD_OPTIONS="$BUILD_OPTIONS --without-tiff"
fi

if test x$MAKEINFO == "xno"; then
   BUILD_OPTIONS="$BUILD_OPTIONS --without-makeinfo"
fi

AC_OUTPUT(Makefile)
AC_OUTPUT([restaurant], [chmod +x restaurant])
