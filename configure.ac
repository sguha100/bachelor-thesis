AC_INIT(reltool,1.3)
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign subdir-objects])

AC_PROG_CXX

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
  AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
  AC_MSG_ERROR([You must install OCaml findlib (the ocamlfind
  command)])
fi

AC_CHECK_OCAML_PKG([ocamlgraph])
if test "$OCAML_PKG_ocamlgraph" = "no"; then
  AC_MSG_ERROR([Please install OCaml findlib module 'ocamlgraph'.])
fi

AC_CHECK_LIB([udbm], [dbm_init], [],
[AC_MSG_ERROR([Please install the UPPAAL DBM library.])])

AC_CHECK_PROG([NOTANGLE], [notangle], [yes], [no])
if test "$NOTANGLE" = "no"; then
  AC_MSG_ERROR([Please install the program notangle.])
fi

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
