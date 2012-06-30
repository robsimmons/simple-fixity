
structure Test = 
struct

   (* We can create a fixity resolver that takes strings to strings *)
   structure FS = 
   IntFixityFn (type tok = string type result = string)

   fun testS resolver s =
      FS.resolveList resolver (String.tokens Char.isSpace s)
   fun testIgnS resolver s = 
      FS.resolve resolver (Stream.fromList (String.tokens Char.isSpace s))
    handle FS.Complete _ => false


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
      FI.AdjResolver 
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
   val () = expect_success "1 2 3 4 5" (1 * 2 * 3 * 4 * 5)
   val () = expect_success "1 + 2 3 4 5" (1 + 2 * 3 * 4 * 5)
   val () = expect_success "1 2 + 3 4 5" (1 * 2 + 3 * 4 * 5)
   val () = expect_success "1 2 3 + 4 5" (1 * 2 * 3 + 4 * 5)
   val () = expect_success "1 2 3 4 + 5" (1 * 2 * 3 * 4 + 5)
   val () = expect_success "1 + 2 + 3 + - 4 + 5" (1 + 2 + 3 + ~ 4 + 5)
   val () = expect_success "1 2 + 3 + - 4 + 5" (1 * 2 + 3 + ~ 4 + 5)
   val () = expect_success "1 + 2 3 + - 4 + 5" (1 + 2 * 3 + ~ 4 + 5)
   val () = expect_success "1 + 2 + 3 - 4 + 5" (1 + 2 + 3 * ~ 4 + 5)
   val () = expect_success "1 + 2 + 3 + - 4 5" (1 + 2 + 3 + ~ 4 * 5)
   val () = expect_success "1 2 3 - 4" (1 * 2 * 3 * ~ 4)
   val () = expect_success "- 1 2 - 3 4" (~ 1 * 2 * ~ 3 * 4)
   val () = expect_error "x + y" (fn Option.Option => true | _ => false)
   val () = expect_error "" 
      (fn FI.Incomplete (NONE, _) => true | _ => false)
   val () = expect_error "4 +" 
      (fn FI.Incomplete (SOME "+", _) => true | _ => false)
   val () = expect_error "- 4 + - - -" 
      (fn FI.Incomplete (SOME "-", _) => true | _ => false)
   val () = expect_success "4 + - 4" 0
   val () = expect_error "4 - + 4" 
      (fn FI.Wrong (SOME "-", "+") => true | _ => false)
   val () = expect_error "+ 4" 
      (fn FI.Wrong (NONE, "+") => true | _ => false)

   (* Equality is defined as non-fix, so '4 = 5 = 10' is an error. *)
   val () = expect_error "4 = 5 = 6" 
      (fn FI.Ambiguous ("=", SOME FI.NON, "=", SOME FI.NON) => true 
        | _ => false)



   (* EXAMPLE 2: Ambiguous parsing
    * 
    * Example 1 had very little opportunity for ambiguous parsing:
    * essentially, consecutive equalities like '4 = 5 = 10', which
    * could be either '(4 = 5) = 10' or '4 = (5 = 10)', were it. 
    *
    * Ambiguity arises due to the NON fixity, prefix operators of
    * different precedences that aren't all the highest precedence,
    * and multiplie NON/LEFT/RIGHT fixities at the same precedence. So
    * we'll do all those things wrong to demonstrate how ambiguity is
    * handled. This also demonstrates the NoAdjResolver
    * constructor. *)

   exception Adjacent of string * string
   val res_pref = 
      FS.NoAdjResolver 
         {adj = fn (t1, t2) => Adjacent (t1, t2),
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

                   | "<->" => Sum.INR (FS.Infix
                                        (3, fn x => fn y => "("^x^"<->"^y^")"))
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
        (fn () => let in (testIgnS res_pref s) handle exn => hnd exn end)
        ("'"^s^"' (error expected)")

   (* We're not handling adjecency, raising Domain if it occurs. *)

   exception Amb = FS.Ambiguous
   val () = expect_error "~ y * x z" 
      (fn Adjacent ("x", "z") => true | _ => false)

   (* The normal case of where ambiguity occurs is a fixity
    * conflict. 
    * 
    * I used to think of "Nonfix" as being
    * not-allowed-to-be-either-left-or-right-associative, but by
    * declaring two successive nonfix operators to be ambiguous, we
    * take the opposite view: that Infix is just unspecified
    * left-or-right, so that "a <-> b <-> c" can parse as either "a
    * <-> (b <-> c)" or "(a <-> b) <-> c". *)

   val () = expect_success "a -> b -> c"   "(a->(b->c))"
   val () = expect_success "a <- b <- c"   "((a<-b)<-c)"
   val () = expect_error "a -> b <- c ^ z" 
      (fn Amb ("->",SOME FS.RIGHT,"<-",SOME FS.LEFT) => true | _ => false)
   val () = expect_error "a <- b -> c ^ z" 
      (fn Amb ("<-",SOME FS.LEFT,"->",SOME FS.RIGHT) => true | _ => false)
   val () = expect_error "a <-> b -> c ^ z" 
      (fn Amb ("<->",SOME FS.NON,"->",SOME FS.RIGHT) => true | _ => false)
   val () = expect_error "a <-> b <- c ^ z" 
      (fn Amb ("<->",SOME FS.NON,"<-",SOME FS.LEFT) => true | _ => false)
   val () = expect_error "a <- b <-> c ^ z" 
      (fn Amb ("<-",SOME FS.LEFT,"<->",SOME FS.NON) => true | _ => false)
   val () = expect_error "a -> b <-> c ^ z" 
      (fn Amb ("->",SOME FS.RIGHT,"<->",SOME FS.NON) => true | _ => false)

   (* The "^ z" suffix above is to supposed to force reduction, which
    * means that the ambiguity error will actually be revealed. We
    * would like have the following generate an error, but the error
    * is not actually generalized until we finalize.
  
   val () = expect_error "a -> b <-> c" 
      (fn Amb ("->",SOME FS.RIGHT,"<->",SOME FS.NON) => true | _ => false)
   val () = expect_error "a <-> b <-> c" 
      (fn Amb ("<->",SOME FS.NON,"<->",SOME FS.NON) => true | _ => false) 

    * It would be a reasonable thing to change to try and make this
    * example raise an error (that is, before finalization). *)

   (* The fixity resolver allows for low-precedence prefix operators
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
    * and infix operators at the same precedence anyway, just as it
    * probably isn't a good idea to have different-associating infix
    * operators at the same precedence. The exception that gets
    * raised, FS.Ambiguous (abbreviated to Amb), is the same. *)

   val () = expect_success "x * ~ y"       "(x*(~y))"
   val () = expect_error "~ y * x ^ z"
      (fn Amb ("~", NONE, "*", SOME FS.NON) => true | _ => false)

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
      (fn Amb ("~", NONE, "!", NONE) => true | _ => false)
   val () = expect_error "~ ! ~ X" 
      (fn Amb ("~", NONE, "!", NONE) => true | _ => false)
   val () = expect_error "! ~ ! X" 
      (fn Amb ("~", NONE, "!", NONE) => true | _ => false)
   val () = expect_error "X ^ ! Y" 
      (fn Amb ("^", SOME FS.NON, "!", NONE) => true | _ => false)
               


   
   (* EXAMPLE 3: Incremental parsing 
    * 
    * I tried to prevent this implementation from being weirdly
    * general, but I did make it so that it would support incremental
    * input: taking one input line at a time from the user, and
    * raising errors as soon as they arise. *)

   fun testN ss =
   let 
      fun tokenize s = Stream.fromList (String.tokens Char.isSpace s)

      fun dispatch (Sum.INL total_state) s = 
             FS.resumeTotal res_pref total_state (tokenize s)
        | dispatch (Sum.INR partial_state) s = 
             FS.resumePartial res_pref partial_state (tokenize s)
   
      fun loop state ss = 
         case ss of 
            [] => raise Fail "Invariant ???"
          | [ s ] => 
            let in
               dispatch state s
             handle FS.Complete total_state => total_state
            end
          | s :: ss =>
            let in
               dispatch state s
             handle FS.Complete st => loop (Sum.INL st) ss
                  | FS.Incomplete (_, st) => loop (Sum.INR st) ss
            end
   in 
      case ss of 
         [] => raise Fail "Invariant ???"
       | [ s ] => 
         let in
            FS.resolve res_pref (tokenize s)
          handle FS.Complete st => st
         end
       | s :: ss => 
         let in
            FS.resolve res_pref (tokenize s)
          handle FS.Complete st => loop (Sum.INL st) ss
               | FS.Incomplete (_, st) => loop (Sum.INR st) ss
         end
   end

   fun expect_success ss expected = 
      Testing.expect ()
        (fn () => let in (FS.finalize (testN ss) = expected) 
                   handle exn => false end)
        ("'"^String.concatWith"/"ss^"' (expected '"^expected^"')")

   (* In expect_error, the stack is never finalized. This is for
    * emphasis: the finalize function rases only Ambiguous errors, and
    * arguably it shouldn't even do that (see the commented-out "a <->
    * b <-> c" error above for a discussion). The implementation
    * tries to raise errors as early as possible, in other words. *)

   fun expect_error ss hnd =
      Testing.expect ()
        (fn () => let in (testN ss; false) 
                   handle exn => hnd exn end)
        ("'"^String.concatWith"/"ss^"' (error expected)")

   val () = expect_error ["12","y"] 
      (fn Adjacent ("12", "y") => true | _ => false)
   val () = expect_error ["","12","y"] 
      (fn Adjacent ("12", "y") => true | _ => false)
   val () = expect_error ["","12","","","y"] 
      (fn Adjacent ("12", "y") => true | _ => false)

   val () = expect_success ["~ 12 ^","9"] "((~12)^9)"
   val () = expect_success ["","~ 12 ^","9"] "((~12)^9)"
   val () = expect_success ["~ 12","^","9"] "((~12)^9)"
   val () = expect_success ["~","12 ^","9"] "((~12)^9)"
   val () = expect_success ["","~ 12 ^","9"] "((~12)^9)"
   val () = expect_success ["","~","12 ^","9"] "((~12)^9)"
   val () = expect_success ["","~ 12","^","9"] "((~12)^9)"
   val () = expect_success ["~","12","^","9"] "((~12)^9)"
   val () = expect_success ["","~","12","^","9"] "((~12)^9)"
   val () = expect_success ["","~","","12","","^","9"] "((~12)^9)"

   val () = expect_error ["~ 12 ^"]
      (fn FS.Incomplete (SOME "^", _) => true | _ => false)
   val () = expect_error ["","~ 12 ^"]
      (fn FS.Incomplete (SOME "^", _) => true | _ => false)
   val () = expect_error ["~ 12","^"]
      (fn FS.Incomplete (SOME "^", _) => true | _ => false)
   val () = expect_error ["~","12 ^"]
      (fn FS.Incomplete (SOME "^", _) => true | _ => false)
   val () = expect_error ["","~ 12 ^"]
      (fn FS.Incomplete (SOME "^", _) => true | _ => false)
   val () = expect_error ["","~","12 ^"]
      (fn FS.Incomplete (SOME "^", _) => true | _ => false)
   val () = expect_error ["","~ 12","^"]
      (fn FS.Incomplete (SOME "^", _) => true | _ => false)
   val () = expect_error ["~","12","^"]
      (fn FS.Incomplete (SOME "^", _) => true | _ => false)
   val () = expect_error ["","~","12","^"]
      (fn FS.Incomplete (SOME "^", _) => true | _ => false)
   val () = expect_error ["","~","","12","","^"]
      (fn FS.Incomplete (SOME "^", _) => true | _ => false)




   val () = Testing.report ()
end
