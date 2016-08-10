# -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.69)

AC_INIT(Restaurant Chef IDE, 0.1-CURRENT, cosmonaut.ok@zoho.com)

AC_SUBST(RESTAURANT_VERSION)
RESTAURANT_VERSION='0.1-CURRENT'

AC_SUBST(EMACS_URI)
AC_SUBST(BUILD_DIR)
AC_SUBST(GUI_TOOLKIT)
AC_SUBST(RELEASE_FILES)
AC_SUBST(EMACS)
AC_SUBST(BUILD_OPTIONS)
AC_SUBST(RUBY)
AC_SUBST(BUNDLE)

## veriables definition
EMACS_URI="http://ftp.gnu.org/pub/gnu/emacs/emacs-24.5.tar.gz"
BUILD_DIR=esyscmd(mktemp -u -d /tmp/restaurant-release-tmp.XXXXXX)
RELEASE_FILES="bin bootstrap.el bootstrap.sh data init.el LICENSE rc README.md scripts snippets themes el-get elpa emacs"
EMACS=esyscmd(which emacs)

# Checks for programs.
AC_ARG_WITH(gui,      [  --with-gui=TOOLKIT      use gtk, gtk2, gtk3, motif, athena, no (default gtk)], GUI_TOOLKIT=$withval, GUI_TOOLKIT="gtk")
AC_ARG_WITH(emacs,    [  --with-emacs=path       use the custom path to emacs binary], EMACS=$withval, EMACS=$EMACS)
AC_ARG_WITH(gif,      [  --with-gif              build emacs with/without gif support (default auto)], GIF=$withval, GIF="no")
AC_ARG_WITH(png,      [  --with-png              build emacs with/without png support (default auto)], SBCL_PATH=$withval, PNG="no")
AC_ARG_WITH(jpeg,     [  --with-jpeg             build emacs with/without jped support (default auto)], CLISP_PATH=$withval, JPEG="no")
AC_ARG_WITH(tiff,     [  --with-tiff             build emacs with/without tiff support (default auto)], CCL_PATH=$withval, TIFF="no")
# AC_ARG_WITH(xml2,     [  --without-xml2             build emacs with/without xml2 support (default auto)], ECL_PATH=$withval, XML2="auto")


dnl AC_ARG_WITH(module-dir,
dnl                      [  --with-module-dir=PATH specify location of contrib modules],
dnl                      MODULE_DIR=$withval, MODULE_DIR="${HOME}/.stumpwm.d/modules")

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

dnl STUMPWM_ASDF_DIR="`pwd`"

dnl if test -x "$SBCL_PATH"; then
dnl    SBCL=$SBCL_PATH
dnl    AC_MSG_CHECKING([for sbcl])
dnl    AC_MSG_RESULT($SBCL)
dnl else
dnl    AC_PATH_PROG([SBCL], sbcl,"")
dnl fi

dnl if test -x "$CLISP_PATH"; then
dnl    CLISP=$CLISP_PATH
dnl    AC_MSG_CHECKING([for clisp])
dnl    AC_MSG_RESULT($CLISP)
dnl else
dnl    AC_PATH_PROG([CLISP],clisp,"")
dnl fi

dnl if test -x "$CCL_PATH"; then
dnl    CCL=$CCL_PATH
dnl    AC_MSG_CHECKING([for ccl])
dnl    AC_MSG_RESULT($CCL)
dnl else
dnl    AC_PATH_PROG([CCL],ccl,"")
dnl fi

dnl if test -x "$ECL_PATH"; then
dnl    ECL=$ECL_PATH
dnl    AC_MSG_CHECKING([for ecl])
dnl    AC_MSG_RESULT($ECL)
dnl else
dnl    AC_PATH_PROG([ECL], ecl,"")
dnl fi

dnl if test -x "$LW_PATH"; then
dnl    LW=$LW_PATH
dnl    AC_MSG_CHECKING([for lispworks])
dnl    AC_MSG_RESULT($LW)
dnl else
dnl    AC_PATH_PROG([LW], lw,"")
dnl fi

dnl if test "x$LISP" = "xany"; then
dnl    if test "$SBCL"; then
dnl       LISP=sbcl
dnl    elif test "$CLISP"; then
dnl       LISP=clisp
dnl    elif test "$CCL"; then
dnl       LISP=ccl
dnl    elif test "$ECL"; then
dnl       LISP=ecl
dnl    elif test "$LW"; then
dnl       LISP=lw
dnl    fi
dnl fi

dnl if test "x$LISP" = "xsbcl"; then
dnl    LISP_PROGRAM=$SBCL
dnl elif test "x$LISP" = "xclisp"; then
dnl    LISP_PROGRAM=$CLISP
dnl elif test "x$LISP" = "xccl"; then
dnl    LISP_PROGRAM=$CCL
dnl elif test "x$LISP" = "xecl"; then
dnl    LISP_PROGRAM=$ECL
dnl elif test "x$LISP" = "xlw"; then
dnl    LISP_PROGRAM=$LW
dnl fi

dnl if test "x$LISP_PROGRAM" = "x"; then
dnl    AC_MSG_ERROR([*** No lisp is available.])
dnl fi

dnl AC_MSG_NOTICE([Using $LISP at $LISP_PROGRAM])

dnl # check for makeinfo

dnl AC_CHECK_PROG(MAKEINFO,makeinfo,yes,no)

dnl if test "$MAKEINFO" = "no"; then
dnl    AC_MSG_WARN([Please install makeinfo for the manual.])
dnl fi

dnl AC_CHECK_PROG(XDPYINFO,xdpyinfo,yes,no)
dnl if test "$XDPINFO" = "no"; then
dnl    AC_MSG_ERROR([xdpyinfo is needed for xinerama support.])
dnl fi

dnl # Checks for libraries.

dnl # Checks for header files.

dnl # Checks for typedefs, structures, and compiler characteristics.

dnl # Checks for library functions.
AC_OUTPUT(Makefile)
AC_OUTPUT(bin/restaurant)
dnl AC_OUTPUT(make-image.lisp)
dnl AC_OUTPUT(load-stumpwm.lisp)
