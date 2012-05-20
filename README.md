simple-fixity
=============

An attempt at general-purpose fixity resolution code for Standard
ML. Fixity resolution is a general problem with simple, yet fiddly
solutions, and I wasn't able to find code that was both:

1. Sufficiently general that it could be reused in a variety of 
   applications.

2. Useful in a situation where good error messages are (or might
   eventually be) required.

3. Sufficiently well-written and well-documented that I was able to
   tell what was going on.

The first and second problem are addressed by functorizing the fixity
code; this allows us to throw different exceptions for different
errors that arise during fixity resolution -- client code can then
handle these errors in a way that is informative to the user. If the
input "tok" type includes position information, then the information
thrown back by exceptions will allow.

If you use Fix.resolveStream or Fix.resolveList, one of three things will
happen. 

1. raise Fix.Trailing (tok_opt, partial_state) 
   Either there was no input (tok_opt = NONE) or there was a trailing
   infix operator (tok_opt = SOME tok). This would be recoverable if
   there was more input!

2. raise Fix.Successive (tok_opt, tok) 
   Either there was a leading infix operator (tok_opt = NONE) or two
   fixity operators were inappropriately placed side-by-side. This is
   just wrong, there is no recovery.

3. raise Fix.MixedAssoc (tok1, lrn_option, tok2, lrn) 
   The parse was ambiguous according to the fixity rules given.

4. Either: succesful return (resolveStream/resolveList) or else 
   raise Finished total_state (resolve/resumeTotal/resumePartial)
   The parse successfully returned. If you're parsing progressively,
   then you don't want to 

In order to make the code comprehensible, the implementation is
simplified: in particular, it is limited to prefix and infix
operators. There are lots of comments in the code, and the runtime
invariants can be dynamically checked by changing which of the two
functions in assert.sml is commented out.

