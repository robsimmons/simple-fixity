(* Signature of a fixity resolver that understands infix and prefix. *)

signature FIXITY = 
sig
   type tok (* Input *)
   type result (* Output *)

   
   (* DESCRIBING FIXITY RESOLVERS *)

   type precedence
   datatype lrn = NON | LEFT | RIGHT
   datatype fixity = 
      Prefix of precedence * (result -> result)
    | Infix of precedence * (result -> result -> result)
    | Infixl of precedence * (result -> result -> result)
    | Infixr of precedence * (result -> result -> result)

   (* A resolver tells the fixity code how to work *)
   type resolver 

   val AdjResolver:
      {token: tok -> (result, fixity) Sum.sum, (* Is a token fixity-able? *)
       adj_prec: precedence,                   (* The precedence of adjacency *)
       adj_assoc: lrn,                         (* Adjacency associates how?*)
       adj_tok: tok,                           (* Pseudo-token for adjacency *)
       adj: result -> result -> result}        (* The operation of adjacency *)
      -> resolver

   val NoAdjResolver:
      {token: tok -> (result, fixity) Sum.sum, (* Is a token fixity-able? *)
       adj: tok * tok -> exn}                  (* What should be thrown if two 
                                                  adjacent tokens appear? *)
      -> resolver


   (* EXECUTING FIXITY RESOLVERS *)

   (* Completely resolve fixity *)
   val resolveStream: resolver -> tok Stream.stream -> result
   val resolveList: resolver -> tok list -> result

   (* Resolve fixity as much as possible, but anticipate more input *)
   type partial_state (* Needs more input to resolve fixity *)
   type total_state (* Has enough input to resolve fixity *)
   val resolve: resolver -> tok Stream.stream -> 'a
   val resumeTotal: resolver -> total_state -> tok Stream.stream -> 'a
   val resumePartial: resolver -> partial_state -> tok Stream.stream -> 'a
   val finalize: resolver -> total_state -> result


   (* OUTCOMES *)

   (* Complete total_state
    *
    * End of input reached successfully (allows resumption with
    * resumeTotal or finalization with finalize). If you don't care
    * about resumption, use resolveStream or resolveList. *)
   exception Complete of total_state

   (* Incomplete (tok_option, partial_state)
    *
    * Parse could be saved with more input using resumePartial. 
    *
    * ""        ~~> Incomplete (NONE, -)
    *
    * "2 + 4 +" ~~> Incomplete (SOME "+", -)
    *
    * "3 + ~"   ~~> Incomplete (SOME "~", -) *)
   exception Incomplete of tok option * partial_state
 
   (* Wrong (tok_option, tok)
    *
    * Always due to successive or leading infix operators.
    *
    * "+ 2"     ~~> Wrong (NONE, "+"): "+2"
    *
    * "2 ~ + 6" ~~> Wrong (SOME "~", "+"): "2~+6"
    *
    * "2 + + 6" ~~> Wrong (SOME "+", "+") *)
   exception Wrong of tok option * tok

   (* Ambiguous (tok1, assoc1, tok2, assoc2)
    * 
    * Ambiguous parse: the available precedence information leaves
    * ambiguity (or potential ambiguity) as to how fixity should be
    * resolved, even though there is some correct way to parse.
    *
    * "a <- b -> c" ~~> Ambiguous ("<-", SOME LEFT, "->", SOME RIGHT) 
    *                   Why: "->" and "<-" have identical fixity, 
    *                   could be "(a <- b) -> c" or "a <- (b -> c)".
    *
    * "~ 2 + 4"     ~~> Ambiguous ("~", NONE, "+", SOME RIGHT)
    *                   Why: "~" and "+" have identical precedence,
    *                   could be "(~ 2) + 4" or "~ (2 + 4)".
    *                   (Note that, even if "~" and "+" have 
    *                   identical precedence, "2 + ~ 4" is 
    *                   unambiguous)
    *
    * "~ sin x + y" ~~> Ambiguous ("~", NONE, "+", NONE)
    *                   Why: "~" has higher precedence than "sin",
    *                   "+" has intermediate precedence, could be
    *                   "(~ (sin x)) + y" if we resolve based on
    *                   "~"'s high precedence or "~ (sin (x + y))"
    *                   if we resolve based on "sin"'s low precedence. 
    *                   (Note: this will be an error even for 
    *                   unambiguous parses like "~ sin x", as they
    *                   have potential ambiguity.) 
    * 
    * "a * sin + y" ~~> Ambiguous ("*", NONE, "sin", NONE)
    *                   Why: Same reason, prec("sin") < prec("+")
    *                   < prec("*"). Note that the first argument
    *                   is NONE and not SOME RIGHT. *)
   exception Ambiguous of tok * lrn option * tok * lrn option

end
