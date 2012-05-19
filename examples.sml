CM.make "$SMACKAGE/cmlib/v1/cmlib.cm";
CM.make "sources.cm";
use "../cmlib/tests/testing.sig";
use "../cmlib/tests/testing.sml";

structure Test = 
struct

   (* We can create a fixity resolver that takes strings to strings *)
  
   structure FS = 
   IntFixityFn (type tok = string type result = string)

   fun testS resolver s =
      FS.resolveList resolver (String.tokens Char.isSpace s)


   (* Or one that takes strings to integers *)

   structure FI =
   IntFixityFn (type tok = string type result = int)

   fun testI resolver s =
      FI.resolveList resolver (String.tokens Char.isSpace s)




   (* EXAMPLE 1: Simple arithmetic fixity
    *
    * The usual intuitive examples of fixity involve arithmatic
    * expressions. Here, adjacency is multiplication, and binds more
    * tightly than anything except for unary minus. *)

   val res_arith = 
      FI.adj_resolver 
         {adj = fn x => fn y => x * y,
          adj_prec = 4,
          adj_assoc = FI.RIGHT,
          adj_tok = "*",
          token = fn "=" => Sum.INR 
                               (FI.Infix
                                   (1, fn i => fn j => if i = j then 1 else 0))

                   | "+" => Sum.INR (FI.Infixr (2, fn i => fn j => i+j)) 

                   | "/" => Sum.INR (FI.Infixr 
                                        (3, fn i => fn j => Int.div (i,j)))

                   | "-" => Sum.INR (FI.Prefix (5, fn i => ~i))
                   | x => Sum.INL (valOf (Int.fromString x))}   

   fun expect_success s expected = 
      Testing.expect ()
        (fn () => let in (testI res_arith s = expected) handle exn => false end)
        ("'"^s^"' (expected '"^Int.toString expected^"')")

   fun expect_error s hnd =
      Testing.expect ()
        (fn () => let in (testI res_arith s; false) handle exn => hnd exn end)
        ("'"^s^"' (error expected)")

   val () = expect_success "- 8 / 3" (~3)
   val () = expect_success "8 / - 3" (~3)
   val () = expect_success "- 8 / - 3" (2)
   val () = expect_success "- 4 + - 3 / - 1 + - 9" (~10)
   val () = expect_success "4 + - 3 / - 1 + 9" 16
   val () = expect_success "- 3 + - 2 = - - - 15 / - - 3" 1 
   val () = expect_error "x + y" (fn Option.Option => true | _ => false)
   val () = expect_error "" (fn FI.Trailing (NONE, _) => true | _ => false)
   val () = expect_error "4 +" 
      (fn FI.Trailing (SOME "+", _) => true | _ => false)
   val () = expect_error "- 4 + - - -" 
      (fn FI.Trailing (SOME "-", _) => true | _ => false)
   val () = expect_error "4 - + 4" 
      (fn FI.Successive (SOME "-", "+") => true | _ => false)
   val () = expect_error "+ 4" 
      (fn FI.Successive (NONE, "+") => true | _ => false)

   (* Equality is defined as non-fix, so '4 = 5 = 10' is an error. *)
   val () = expect_error "4 = 5 = 6" 
      (fn FI.MixedAssoc ("=", FI.NON, "=", FI.NON) => true | _ => false)

   (* EXAMPLE 2: Degenerate fixity
    * 
    * To discuss some of the more corner-case aspects of fixity
    * resolution, we'll create a slightly more degenerate example. 
    * This also demonstrates the no_adj_resolver constructor for
    * resolvers. *)

   val res_pref = 
      FS.no_adj_resolver 
         {adj = fn _ => Domain,
          token = fn "!" => Sum.INR (FS.Prefix (1, fn s => "(!"^s^")")) 
                   | "?" => Sum.INR (FS.Prefix (1, fn s => "(?"^s^")"))
                   | "=" => Sum.INR (FS.Infix 
                                        (1, fn x => fn y => "("^x^"="^y^")"))

                   | "^" => Sum.INR (FS.Infix 
                                        (2, fn x => fn y => "("^x^"^"^y^")"))

                   | "~" => Sum.INR (FS.Prefix (3, fn s => "(~"^s^")"))
                   | "-" => Sum.INR (FS.Prefix (3, fn s => "(-"^s^")"))
                   | "*" => Sum.INR (FS.Infix 
                                        (3, fn x => fn y => "("^x^"*"^y^")"))

                   | "<-" => Sum.INR (FS.Infixl
                                        (3, fn x => fn y => "("^x^"<-"^y^")"))
                   | "->" => Sum.INR (FS.Infixr
                                        (3, fn x => fn y => "("^x^"->"^y^")"))
                   | x => Sum.INL x}

   fun expect_success s expected = 
      Testing.expect ()
        (fn () => let in (testS res_pref s = expected) handle exn => false end)
        ("'"^s^"' (expected '"^expected^"')")

   fun expect_error s hnd =
      Testing.expect ()
        (fn () => let in (testS res_pref s; false) handle exn => hnd exn end)
        ("'"^s^"' (error expected)")

   (* We're not handling adjecency, raising Domain if it occurs. *)

   val () = expect_error "~ y * x z" (fn Domain => true | _ => false)
   val () = expect_success "a -> b -> c"   "(a->(b->c))"
   val () = expect_success "a <- b <- c"   "((a<-b)<-c)"
   val () = expect_error "a -> b <- c" 
      (fn FS.MixedAssoc ("->",FS.RIGHT,"<-",FS.LEFT) => true | _ => false)
   val () = expect_error "a <- b -> c" 
      (fn FS.MixedAssoc ("<-",FS.LEFT,"->",FS.RIGHT) => true | _ => false)

   (* Conservative treatment of infix operators
    * 
    * The fixity resolver allows for low-precedence prefix operators
    * to precede higher-fixity infix operators, thus the following
    * parse unambiguously: *)

   val () = expect_success "! ? ! ~ - X"   "(!(?(!(~(-X)))))" 
   val () = expect_success "! ! x * y"     "(!(!(x*y)))" 
   val () = expect_success "- x = ~ y ^ z" "((-x)=((~y)^z))"
   val () = expect_success "~ x ^ y = - z" "(((~x)^y)=(-z))"

   (* It is similarly unambiguous to have 'x * ~ y', when those
    * operators are of the same precedence. However, for '~ x * y',
    * both the parse '((~x)*y)' and '(~(x*y))' have a valid claim to
    * being the legitimate parse, and so the latter case raises an
    * exception. Really, it probably isn't a good idea to have prefix
    * and infix operators at the same precedence anyway. *)

   val () = expect_success "x * ~ y"       "(x*(~y))"
   val () = expect_error "~ y * x"
               (fn FS.PrefixEqualInfix ("~", "*") => true | _ => false)

   (* A more ambiguous case is when a low-precedence prefix operator
    * follows a higher-precedence operator (either infix or
    * prefix). Even though '~ ! X' parses unambiguously as '(~(!X))',
    * we run into a problem if we have '~ ! X ^ Y' -- where '~' is
    * high precedence, '!' is low precedence. Does this parse as
    * '(~(!(X^Y)))', taking the cue from the low-precedence '!', or as
    * '((~(!X))^Y)', taking the cue from the high-precedence '~'?
    *
    * A more sophisticated algorithm might keep track of the range of
    * high-low mismatch and only raise an error when actual ambiguity
    * arose (thus accepting '~ ! X' but not '~ ! X ^ Y'). Our
    * treatment is more conservative: we reject any low-precedence
    * prefix operator that follows a high-precedence operator. This
    * also rejects 'X ^ ! Y', where a low-precedence prefix operator
    * follows a higher-precedence infix operator.
    *
    * Note that, '! ~ X ^ Y' is unambiguous: the '~' binds most
    * tightly, so it parses as '(!((~X)^Y))'. *)
 
   val () = expect_success "! ~ X ^ Y"     "(!((~X)^Y))"
   val () = expect_error "~ ! X ^ Y" 
               (fn FS.SomethingLowPrefix ("~", "!") => true | _ => false)
   val () = expect_error "~ ! ~ X" 
               (fn FS.SomethingLowPrefix ("~", "!") => true | _ => false)
   val () = expect_error "! ~ ! X" 
               (fn FS.SomethingLowPrefix ("~", "!") => true | _ => false)
   val () = expect_error "X ^ ! Y" 
               (fn FS.SomethingLowPrefix ("^", "!") => true | _ => false)
               

   val res = 
      FS.no_adj_resolver 
         {adj = fn _ => Match,
          token = fn "!" => Sum.INR (FS.Prefix (1, fn s => "(!"^s^")")) 
                   | "~" => Sum.INR (FS.Prefix (2, fn s => "(~"^s^")"))
                   | x => Sum.INL x}

(*
   val () = expect_error (fn "!" => SOME (FS.Prefix 1, fn s => "(!"^s^")") 
                           | "~" => SOME (FS.Prefix 2, fn s => "(~"^s^")") 
      (fn () =>
       let in
         ( ()
         ; false) 
        handle FixityString.SomethingLowPrefix ("~", "!") => true
             | _ => false 
       end)
      "'~ ! ~ foo', '!' has lower precedence (exception)"

   val () = Testing.expect () 
      (fn () =>
       let in
         ( ()
         ; false) 
        handle FixityString.SomethingLowPrefix ("!", "~") => true
             | _ => false 
       end)
      "'~ ! ~ foo', where '~' has lower precedence (exception)"
*)

   val () = Testing.report ()
(* ### 
The
second problem is addressed by the fact that the implementation
implements runtime checks that the parse stack is well-formed.


Some parses that are technically non-ambiguous parses, are
disallowed. One example is "~ ! ~ X" where ~ and ! are prefix
operators of different precedence; this would lead to a the exception
SomethingLowPrefix (~, !) if ! has lower precendence and the exception
SomethingLowPrefix (!, ~) if ~ has lower precedence. *)
end
