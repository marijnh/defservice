# URL dispatching macro system

See [http://marijn.haverbeke.nl/defservice/](http://marijn.haverbeke.nl/defservice/)
for the notes to a talk I gave about this.

Read through `test.lisp` to get a feel of how the system is used.

To actually use this in a web server, two pieces of glue have to be
written:

 * Write some kind of parameter reader (example in
   `defservice.agraph.lisp`), that can extract parameters from your
   request.

 * Catch the dispatch-failed conditions, and return an HTTP response
   based on their `code` and `headers` fields.
