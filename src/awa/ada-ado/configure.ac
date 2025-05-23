dnl Autoconf configure script for Ada Database Objects
dnl Copyright (C) 2010 - 2021 Stephane Carrez
dnl Written by Stephane Carrez (Stephane.Carrez@gmail.com)
dnl
dnl  Licensed under the Apache License, Version 2.0 (the "License");
dnl  you may not use this file except in compliance with the License.
dnl  You may obtain a copy of the License at
dnl
dnl     http://www.apache.org/licenses/LICENSE-2.0
dnl
dnl  Unless required by applicable law or agreed to in writing, software
dnl  distributed under the License is distributed on an "AS IS" BASIS,
dnl  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
dnl  See the License for the specific language governing permissions and
dnl  limitations under the License.
dnl 

dnl Process this file with autoconf to produce a configure script.

AC_INIT
AC_CONFIG_SRCDIR([src/ado.ads])

# Current release settings
ADO_MAJOR_VERSION=2
ADO_MINOR_VERSION=3
ADO_MICRO_VERSION=0
ADO_VERSION=$ADO_MAJOR_VERSION.$ADO_MINOR_VERSION.$ADO_MICRO_VERSION

AC_SUBST(ADO_VERSION)
AC_SUBST(ADO_MAJOR_VERSION)
AC_SUBST(ADO_MINOR_VERSION)
AC_SUBST(ADO_MICRO_VERSION)

AM_GNAT_LIBRARY_PROJECT(ado)

AC_CHECK_PROG(HAVE_PANDOC,pandoc,yes)
AC_CHECK_PROG(HAVE_DYNAMO,dynamo,yes)

AC_PROG_CC

AC_ARG_ENABLE(mysql,
[  --enable-mysql          build MySQL support],
ENABLE_MYSQL=$enableval,
ENABLE_MYSQL=yes)

AC_ARG_ENABLE(mariadb,
[  --enable-mariadb        build MariaDB support (MySQL alternative)],
ENABLE_MARIADB=$enableval
ENABLE_MYSQL=$enableval,
ENABLE_MARIADB=yes
ENABLE_MYSQL=yes)

AC_ARG_ENABLE(sqlite,
[  --enable-sqlite         build SQLite support],
ENABLE_SQLITE=$enableval,
ENABLE_SQLITE=yes)

AC_ARG_ENABLE(postgresql,
[  --enable-postgresql     build Postgresql support],
ENABLE_POSTGRESQL=$enableval,
ENABLE_POSTGRESQL=yes)

DB=""
WITH_ADO_MYSQL=""
ADO_HAVE_MYSQL=no

##########################################
# MySQL support
##########################################

ac_mysql_config=mysql_config
AC_MSG_CHECKING([MySQL configuration])
AC_ARG_WITH(mysql,
AS_HELP_STRING([--with-mysql=ARG], [Use the MySQL configuration tool (mysql_config)]),
[
  ac_mysql_config=${withval}
],
[
])
AC_MSG_RESULT(${ac_mysql_config})

ac_mysql_libs=`$ac_mysql_config --libs 2>/dev/null`

if test "${ENABLE_MYSQL}" = "yes" ; then
  case "${host}" in
  ## On Windows, the MySQL library name is libmysql
  *mingw* )
    MYSQL_LIB=mysql

    AC_CHECK_PROGS(MYSQL, mysql)
    if test "X$MYSQL" != "X" ; then
       DB="$DB mysql"
       ADO_HAVE_MYSQL=yes
	   WITH_ADO_MYSQL="with \"ado_mysql\";";
       MYSQL_LIBNAME="-l$MYSQL_LIB"
    fi
    ;;

  *)
    if test "T$ac_mysql_libs" != "T"; then
      LIBS="$ac_mysql_libs"
      AC_LINK_IFELSE(
         [AC_LANG_PROGRAM([extern int mysql_init(char*);],
                          [return mysql_init ("")])],
         [DB="$DB mysql"
          ADO_HAVE_MYSQL=yes
          WITH_ADO_MYSQL="with \"ado_mysql\";";
          MYSQL_LIBNAME="$ac_mysql_libs"
          MYSQL_LIBS="$ac_mysql_libs"],
         [ADO_HAVE_MYSQL=no])
    else
      MYSQL_LIB=mysqlclient
      AC_SEARCH_LIBS([mysql_init],[$MYSQL_LIB mariadb mariadbclient],[
        DB="$DB mysql"
        ADO_HAVE_MYSQL=yes
        WITH_ADO_MYSQL="with \"ado_mysql\";";
        MYSQL_LIBNAME="$ac_cv_search_mysql_init"
        MYSQL_LIBS="$ac_cv_search_mysql_init"
        ])
    fi
    ;;
  esac
fi

WITH_ADO_SQLITE=""
ADO_HAVE_SQLITE=no
if test "${ENABLE_SQLITE}" = "yes" ; then
  AC_CHECK_LIB(sqlite3,sqlite3_open,[
    DB="$DB sqlite"
    ADO_HAVE_SQLITE=yes
    WITH_ADO_SQLITE="with \"ado_sqlite\";";
  ])

fi
if test "${ADO_HAVE_SQLITE}" = "yes" ; then
  AC_CHECK_PROG(SQLITE3_CHECK,sqlite3,yes)
  if test x"$SQLITE3_CHECK" != x"yes" ; then
    AC_MSG_ERROR([Please install sqlite3 command line to use SQLite.])
  fi
fi

WITH_ADO_POSTGRESQL=""
ADO_HAVE_POSTGRESQL=no
if test "${ENABLE_POSTGRESQL}" = "yes" ; then
  AC_CHECK_LIB(pq,PQexec,[
    DB="$DB postgresql"
    ADO_HAVE_POSTGRESQL=yes
    WITH_ADO_POSTGRESQL="with \"ado_postgresql\";";
  ])
fi
if test "${ADO_HAVE_POSTGRESQL}" = "yes" ; then
  AC_CHECK_PROG(PSQL_CHECK,psql,yes)
  if test x"$PSQL_CHECK" != x"yes" ; then
    AC_MSG_ERROR([Please install psql command line to use Postgresql.])
  fi
fi


ADO_TESTS_HARNESS=''
ADO_TESTS_SRC_DIR=''
ADO_DATABASES_SRC_DIR=''
if test -z "$DB"; then
  AC_MSG_ERROR([No database supported.  Try to install mysql, sqlite or postgresql development libraries])
else
  ADO_DATABASES_SRC_DIR=
  for i in $DB; do
    ADO_DATABASES_SRC_DIR="$ADO_DATABASES_SRC_DIR, \"src/$i\""
    ADO_TESTS_SRC_DIR="$ADO_TESTS_SRC_DIR, \"regtests/$i\""
    if test ! -z "$ADO_TESTS_HARNESS"; then
       ADO_TESTS_HARNESS="$ADO_TESTS_HARNESS, "
    fi
    ADO_TESTS_HARNESS="$ADO_TESTS_HARNESS \"ado_${i}_harness\""
  done
fi

AC_SUBST(ADO_DATABASES_SRC_DIR)
AC_SUBST(ADO_TESTS_SRC_DIR)
AC_SUBST(ADO_TESTS_HARNESS)
AC_SUBST(ADO_HAVE_MYSQL)
AC_SUBST(ADO_HAVE_SQLITE)
AC_SUBST(ADO_HAVE_POSTGRESQL)
AC_SUBST(MYSQL_LIBNAME)
AC_SUBST(MYSQL_LIBS)
AC_SUBST(WITH_ADO_MYSQL)
AC_SUBST(WITH_ADO_SQLITE)
AC_SUBST(WITH_ADO_POSTGRESQL)

AM_GNAT_FIND_ADA_UTIL([Ada ADO])

##########################################
# XMLAda library
##########################################

AM_GNAT_FIND_XML_ADA([], [
   AC_MSG_WARN(XML/Ada was not found.)
   AC_MSG_ERROR([Please install XML/Ada, re-configure and re-build Ada Utility Library.])
 ])

AC_ARG_ENABLE(link-options-mysql,
[  --enable-link-options-mysql=opts
                          Add some linker options when building the MySQL driver shared library (disabled)],
[
  ADO_MYSQL_LIBRARY_OPTIONS=${enableval}
])
AC_MSG_CHECKING([for additional linker options for libada_ado_mysql])
if test "T${ADO_MYSQL_LIBRARY_OPTIONS}" != T; then
    ADO_MYSQL_LIBRARY_OPTIONS="\"-Wl,${ADO_MYSQL_LIBRARY_OPTIONS}\""
    opts=${ADO_MYSQL_LIBRARY_OPTIONS}
else
    opts=none
fi
AC_MSG_RESULT($opts)
AC_SUBST(ADO_MYSQL_LIBRARY_OPTIONS)

AC_ARG_ENABLE(link-options-sqlite,
[  --enable-link-options-sqlite=opts
                          Add some linker options when building the SQLite driver shared library (disabled)],
[
  ADO_SQLITE_LIBRARY_OPTIONS=${enableval}
])
AC_MSG_CHECKING([for additional linker options for libada_ado_sqlite])
if test "T${ADO_SQLITE_LIBRARY_OPTIONS}" != T; then
    ADO_SQLITE_LIBRARY_OPTIONS="\"-Wl,${ADO_SQLITE_LIBRARY_OPTIONS}\""
    opts=${ADO_SQLITE_LIBRARY_OPTIONS}
else
    opts=none
fi
AC_MSG_RESULT($opts)
AC_SUBST(ADO_SQLITE_LIBRARY_OPTIONS)

AC_ARG_ENABLE(link-options-postgresql,
[  --enable-link-options-postgresql=opts
                          Add some linker options when building the Postgresql driver shared library (disabled)],
[
  ADO_POSTGRESQL_LIBRARY_OPTIONS=${enableval}
])
AC_MSG_CHECKING([for additional linker options for libada_ado_postgresql])
if test "T${ADO_POSTGRESQL_LIBRARY_OPTIONS}" != T; then
    ADO_POSTGRESQL_LIBRARY_OPTIONS="\"-Wl,${ADO_POSTGRESQL_LIBRARY_OPTIONS}\""
    opts=${ADO_POSTGRESQL_LIBRARY_OPTIONS}
else
    opts=none
fi
AC_MSG_RESULT($opts)
AC_SUBST(ADO_POSTGRESQL_LIBRARY_OPTIONS)

AC_CONFIG_FILES([Makefile.conf
ado.gpr
ado_tests.gpr
samples.gpr
ado_mysql.gpr
ado_sqlite.gpr
ado_postgresql.gpr
ado_all.gpr
])
AC_OUTPUT
AC_MSG_NOTICE([supported databases: $DB])
