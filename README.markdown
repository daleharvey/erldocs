Erldocs
=======

This is the code used to generate documentation for erlang projects in the format of [erldocs.com](http://erldocs.com)

Dependencies
============

Erlang R13B04 or greater

Building
========

        git clone git://github.com/daleharvey/erldocs.git
        cd erldocs
        make

or download [https://github.com/daleharvey/erldocs/raw/master/erldocs](https://github.com/daleharvey/erldocs/raw/master/erldocs) and place it in your $PATH

Usage
=====

Calling the erldocs script with no arguments will generate documentation for the application in the current working. The documentation will be output to "doc/erldocs".

`./erldocs`

Additional arguments can specify the location to source files to be documented

`./erldocs path/to/erlang/otp/lib/* path/to/erlang/erts`

You can specify the output directory with the `-o` flag

`./erldocs -o path/to/output path/to/erlang/otp/lib/* path/to/erlang/erts`
