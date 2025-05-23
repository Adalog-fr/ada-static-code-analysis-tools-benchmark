# Create Makefile.conf

# As of gnat pro 21, gnat_util is no longer provided or required

echo 'with "gnat_util"; abstract project check is end check;' > check.gpr
gprbuild -P check.gpr > /dev/null 2>&1
if test $? -eq 0 ; then
    echo 'HAVE_GNAT_UTIL := "yes"' >> Makefile.conf
else
    echo 'HAVE_GNAT_UTIL := "no"' >> Makefile.conf
fi

# end of file
