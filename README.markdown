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

or download 

        https://github.com/daleharvey/erldocs/raw/master/rebar

and place it in your $PATH

Usage
=====

Calling the erldocs script with no arguments will generate documentation for the application in the current working. The documentation will be output to "doc/erldocs".

`./erldocs`

Calling erldocs with one argument will changes the output directory to the one specified in the argument.

`./erldocs docs/alternate_location`

Calling erldocs with multiple arguments changes both the destination of the generated documentation and the source of the documentation.

`./erldocs path/to/erlang/otp/lib/edoc doc/edoc_docs`

Source arguments can use wildcards.

`./erldocs path/to/erlang/otp/lib/* doc/otp_docs`
