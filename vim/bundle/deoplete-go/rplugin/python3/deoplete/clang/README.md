python3-libclang
================

This is a port of the `clang` Python bindings to Python 3. I strive to be as
close as possible to the Python 2 bindings; in fact, the unit tests are
exactly the same except for one minor difference in `io` (which is due to
changes in the Python API.)

This release of the library is compatible with Clang 3.6.

You may need to alter LD_LIBRARY_PATH so that the Clang library can be
found. The unit tests are designed to be run with 'nosetests'. For example:


    $ LD_LIBRARY_PATH=$(llvm-config-3.6 --libdir) nosetests3 -v

Status
------

This build is synced against 3.6 upstream.

Installation
------------

On Ubuntu, you need the `python3-nose` package. You can install it using
`sudo apt-get install python3-nose`. Alternatively, you can download it using
pip.
