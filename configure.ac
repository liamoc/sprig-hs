AC_INIT([Haskell Sprig Package], [0.1], [liamoc@cse.unsw.edu.au], [sprig])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([includes/HsSDLConfig.h.in])

# Header file to place defines in
AC_CONFIG_HEADERS([includes/HsSDLConfig.h])

AC_PATH_PROGS([SDL_CONFIG], [sdl-config sdl11-config], [none])

if test "x$SDL_CONFIG" = "xnone"; then
   
   AC_MSG_ERROR([*** SDL not found! Get SDL from www.libsdl.org.
If you already installed it, check it's in the path. If problem remains,
please send a mail to the address that appears in ./configure --version
indicating your platform, the version of configure script and the problem.])

fi

AC_CHECK_LIB([sprig],
             [SPG_NumErrors],
                  [if test -n "$LDPREFIX" -a -r `$SDL_CONFIG --prefix`/lib/libsprig.a
then SDL_LIBS=`$SDL_CONFIG --prefix`/lib/libsprig.a
else SDL_LIBS=-lsprig
fi],
             [AC_MSG_ERROR([*** Sprig not found!
/])])
SDL_CFLAGS=`$SDL_CONFIG --cflags`

AC_SUBST([SDL_CFLAGS])
AC_SUBST([SDL_LIBS])

AC_CONFIG_FILES([config.mk sprig.buildinfo])
AC_OUTPUT
