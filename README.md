bachelor-thesis
===============

Pre-installation steps:

- Get G++, Make, Ocaml, Git, Noweb and Ocamlgraph.

- Get the UPPAAL DBM library, from
  https://github.com/airbornemihir/uppaal-dbm. v2.0.8-reltool1 is
  known to work; you can download the
  tarball(https://github.com/airbornemihir/uppaal-dbm/archive/v2.0.8-reltool1.tar.gz)
  or a zip archive
  (https://github.com/airbornemihir/uppaal-dbm/archive/v2.0.8-reltool1.zip).

Installation steps:

- run
$ LIBS="-lstdc++" ./configure
You might have to use the LDFLAGS and CPPFLAGS variables to specify the locations of your UPPAAL library and headers, for instance:
$ LDFLAGS="-L$HOME/uppaal/lib" CPPFLAGS="-I$HOME/uppaal/include" LIBS="-lstdc++" ./configure
After this, you can run
$ make
$ make install
Note that VPATH builds are not supported for now.

- Now, try running an example. Here, we select breaking2.txt. Run 'calc < grammar-noweb/examples/breaking2.txt'.

- This generates 4 files in /tmp.
-- /tmp/lts.txt
-- /tmp/lts.dot
-- /tmp/lts_quotient.dot
-- /tmp/quotient_lts.dot
Of these, the last 3 can be processed with graphviz to get png images.