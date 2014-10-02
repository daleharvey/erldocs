# To Do

* Search function
    * See Unicode support
    * See quoted || non-alnum atoms support
    * Look at minifying || pref
* [Fix links in Types](//github.com/erldocs/erldocs/issues/16)
    * …don't work between most applications
    * except for example [OTP's zip module](//erldocs.com/17.3/stdlib/zip.html#type-zip_file)
        * It's code: `<seealso marker="kernel:file#read_file_info/1">file:read_file_info/1</seealso>`
        * The `kernel:` part is missing most times.
        * …it links to `../kernel/file.htm#…`
        * MAY imply fixing `specs_gen`'s output & OTP's `doc/src`
* Types pretty printing misaligned
    * eg: `zip#type-zip_file/0` (alignment using whitespace)
    * Already hackishly prepending `&nbsp;`
    * MAY imply re-parsing `specs_gen`'s output
* Condense type specs
    * based on multiple-clauses and/or same specs
* Fix functions & type anchors
    * Look at `edoc`
* [Redirect OTP's manual pages to erlang.org](//github.com/erldocs/erldocs/issues/1)
* Convert
    * `<anno></anno>` to nothing?
    * `<taglist> (<tag>|<item>)+` to `<li><ul>` (with indentation)
* Boldify (instead of italic-ing) bold stuff in the first `descr` part
* Have the same syntax coloring rules as emacs as it is the go-to formating tool
* Add types to the search box somehow
* Fix OTP's edoc issues reported in [the daily build](//erldocs.com/maint/log-maint.txt)
