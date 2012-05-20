simple-fixity
=============

An attempt at general-purpose fixity resolution code for Standard
ML. Fixity resolution is a general problem with a simple, yet fiddly
solutions, and I wasn't able to find code that was simultaneously:

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
carried by exceptions will allow for good error reporting.

In order to make the code comprehensible, the implementation is
simplified: in particular, it is limited to prefix and infix operators
(no postfix operators). There are lots of comments in the code, and
the runtime invariants can be dynamically checked or not by changing
which of the two functions in [assert.sml][1] is commented out. A
couple of simple examples of using the fixity resolver are given in
[examples.sml][2].

Initializing simple-fixity
--------------------------

Assuming that you want precedence to be an integer, then you want to 
instantiate the fixity resolving structure like this:

```sml
structure Fix = 
IntFixityFn (type tok = (* my type of input tokens *)
             type result = (* my type of output tree-structured data *))
```

See [here][3] for two examples of instantiating this functor. If you
have some different notion of precedence, then you will want to
instantiate the functor like this, using an instance of the [ORDERED
signature][4] from CMLib:

```sml
structure Fix = 
SimplestFixityFn (structure Precedence = (* Some instance of ORDERED *)
                  type tok = (* my type of input tokens *)
                  type result = (* my type of output tree-structured data *))
```

You need to build an element of the `Fix.resolver` type to resolve
fixity. `Fix.AdjResolver` builds a resolver that allows adjacent
non-fixity tokens, and `Fix.NoAdjResolver` builds a resolver that
doesn't allow that adjacent non-fixity tokens.

Using simple-fixity
-------------------

Any partial fixity-resolution problem is in one of four states: it is
(potentially) Complete, Incomplete, Wrong, or (potentially)
Ambiguous. These four states correspond to exceptions that may be
thrown by the functions `Fix.resolve`, `Fix.resumeTotal`,
`Fix.resumePartial`.

1. `raise Fix.Complete` 

   The parse successfully returned. If you're parsing progressively,
   then you don't want to reduce "5 + 9" to 14 if the next tokens
   you're going to get are "*" and "2". 

   If you don't care about resumption, use `Fix.resolveStream` or
   `Fix.resolveList`, which handle this exception internally and just
   return the answer.

2. `raise Fix.Incomplete`

   Either there was no input or there was a trailing infix
   operator. This would be recoverable if there was more input using
   `Fix.resumePartial`.

3. `raise Fix.Wrong`

   A leading infix operator or an infix operator following an infix or
   prefix operator. This is just wrong, there is no recovery.

4. `raise Fix.Ambiguous`

   The parsing problem seems to have more than one valid solution: an
   exception is conservatively thrown in lieu of doing something
   arbitrary. 

   This exception can be completely avoided if all prefix operators
   have the same precedence that is higher than all other precedences,
   if all infix operators are either `LEFT` or `RIGHT` associative
   (not `NON` associative), and if `LEFT` and `RIGHT` associative
   operators never have the same precedence.

More documentation is given in the [FIXITY signature][5]

[1]: https://github.com/robsimmons/simple-fixity/blob/master/assert.sml
[2]: https://github.com/robsimmons/simple-fixity/blob/master/examples.sml
[3]: https://github.com/robsimmons/simple-fixity/blob/master/examples.sml#L10-25
[4]: https://github.com/standardml/cmlib/blob/master/ordered.sig
[5]: https://github.com/robsimmons/simple-fixity/blob/master/fixity-sig.sml