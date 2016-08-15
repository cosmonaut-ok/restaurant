# -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.69)

AC_INIT(Restaurant Chef IDE, esyscmd(grep autoconf-anchor rc/version.el | cut -d\" -f2 | tr -d \\n), cosmonaut.ok@zoho.com)

AC_SUBST(EMACS_URI)
AC_SUBST(BUILD_DIR)
AC_SUBST(GUI_TOOLKIT)
AC_SUBST(RELEASE_FILES)
AC_SUBST(EMACS)
AC_SUBST(BUILD_OPTIONS)

## veriables definition
EMACS_URI=https://github.com/emacs-mirror/emacs/archive/emacs-25.1-rc1.tar.gz
BUILD_DIR=/tmp/restaurant-release
RELEASE_FILES="restaurant bootstrap.el data init.el LICENSE rc README.md el-get elpa emacs share Gemfile"
BUILD_OPTIONS="--without-pop"

# Checks for programs.
AC_ARG_WITH(gui,      [  --with-gui=TOOLKIT      use gtk2, gtk3, motif, athena, no (default gtk3)], GUI_TOOLKIT=$withval, GUI_TOOLKIT="gtk3")
AC_ARG_WITH(emacs,    [  --with-emacs=path       use the custom path to emacs binary], EMACS=$withval, EMACS=$EMACS)
AC_ARG_WITH(gif,      [  --with-gif              build emacs with/without gif support (default no)], GIF=$withval, GIF="no")
AC_ARG_WITH(png,      [  --with-png              build emacs with/without png support (default no)], PNG=$withval, PNG="no")
AC_ARG_WITH(jpeg,     [  --with-jpeg             build emacs with/without jped support (default no)], JPEG=$withval, JPEG="no")
AC_ARG_WITH(tiff,     [  --with-tiff             build emacs with/without tiff support (default no)], TIFF=$withval, TIFF="no")
AC_ARG_WITH(makeinfo,     [  --with-makeinfo         build emacs with/without makeinfo additional information (default no)], MAKEINFO=$withval, MAKEINFO="no")
# AC_ARG_WITH(xml2,     [  --without-xml2             build emacs with/without xml2 support (default no)], ECL_PATH=$withval, XML2="auto")

# check for makeinfo
AC_CHECK_PROG(GIT,git,yes,no)
AC_CHECK_PROG(MERCURIAL,hg,yes,no)
AC_CHECK_PROG(RUBY,ruby,yes,no)
AC_CHECK_PROG(BUNDLE,bundle,yes,no)

if test $GIT == "no"; then
   AC_MSG_ERROR([there is no git package in system])
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
AC_OUTPUT(Gemfile)

