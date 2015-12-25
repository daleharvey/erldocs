# erldocs [![Build Status](//travis-ci.org/erldocs/erldocs.svg?branch=master)](//travis-ci.org/erldocs/erldocs)
This is the code used to generate documentation for erlang projects in the format of
[erldocs.com](http://erldocs.com)



## Dependencies
Erlang/OTP ≥ R13B04

## Building

    git clone https://github.com/erldocs/erldocs.git erldocs.git
    cd erldocs.git/
    make -j

An [escript](http://www.erlang.org/doc/man/escript.html) called `erldocs` will thus be generated.

## Usage

***Mind the space around the flags!***

    erldocs  [-o ‹output dir›]  ‹source path›⁺

Calling the script to generate documentation for the application in the current working directory:
this documentation will be output to "./doc/erldocs".

    ./erldocs .

Additional arguments can specify the location to source files to be documented

    ./erldocs path/to/erlang/otp/lib/* path/to/erlang/erts

You can specify the output directory with the `-o` flag

    ./erldocs -o path/to/output path/to/erlang/otp/lib/* path/to/erlang/erts

Include files are automatically found, wherever they are inside `my_app`

    ./erldocs my_app/ -o my_app/doc/

To build Erlang|OTP's docs the same way [erldocs.com](http://erldocs.com/) builds them:

    ./otp.sh otp_src_17.0/
