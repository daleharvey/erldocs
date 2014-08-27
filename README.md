# erldocs
This is the code used to generate documentation for erlang projects in the format of
[erldocs.com](http://erldocs.com)

## Dependencies
Erlang/OTP ≥ R13B04

## Building

    git clone https://github.com/erldocs/erldocs.git erldocs.git
    cd erldocs.git/
    make

An [escript](http://www.erlang.org/doc/man/escript.html) called `erldocs` will thus be generated.

## Usage

***Mind the space around the flags!***

    erldocs  [-o ‹output dir›]  [-I ‹include path›]⁺  ‹source path›⁺

Calling the script to generate documentation for the application in the current working directory:
this documentation will be output to "./doc/erldocs".

    ./erldocs .

Additional arguments can specify the location to source files to be documented

    ./erldocs path/to/erlang/otp/lib/* path/to/erlang/erts

You can specify the output directory with the `-o` flag

    ./erldocs -o path/to/output path/to/erlang/otp/lib/* path/to/erlang/erts

Specify paths to include files with `-I ‹path to include file1›`

    ./erldocs -I lib/stdlib/include -I ./include src/

To build Erlang|OTP's docs the same way [erldocs.com](http://erldocs.com/) builds them:

    ./otp.sh otp_src_17.0/
