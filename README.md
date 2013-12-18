# erldocs
This is the code used to generate documentation for erlang projects in the format of
[erldocs.com](http://erldocs.com)

## Dependencies
R13B04 ≤ Erlang/OTP
You need **rebar** in your `$PATH`

## Building

    git clone git://github.com/fenollp/erldocs.git
    cd erldocs/
    make

## Usage

Calling the erldocs script with no arguments will generate documentation for the application in the current working directory. The documentation will be output to "doc/erldocs".

    ./erldocs

Additional arguments can specify the location to source files to be documented

    ./erldocs path/to/erlang/otp/lib/* path/to/erlang/erts

You can specify the output directory with the `-o` flag

    ./erldocs -o path/to/output path/to/erlang/otp/lib/* path/to/erlang/erts

## To Do
* Add include flags in the command-line. (Would allow `-I lib/*/include/*` and fix some erldocs.com 404s)
