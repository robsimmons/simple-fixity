simple-fixity
=============

An attempt at general-purpose fixity resolution code for Standard
ML. Fixity resolution is a general problem with simple, yet fiddly
solutions, and I wasn't able to find code that was both:

 # Sufficiently general that it could be reused.

 # Useful in a situation where good error messages are (or might
   eventually be) required.

 # Sufficiently well-written and well-document that I was able to tell
   what was going on.

The first and second problem are addressed by functorizing the fixity
code; this allows us to throw different exceptions for different
errors that arise during fixity resolution -- client code can then
handle these errors in a way that is informative to the user.

In order to make the code comprehensible, the implementation is
simplified: in particular, it is limited to prefix and infix
operators. There are lots of comments in the code, and the runtime
invariants can be dynamically checked by changing which of the two
functions in assert.sml is commented out.

