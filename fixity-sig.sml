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

   val adj_resolver:
      {token: tok -> (result, fixity) Sum.sum,
       adj_prec: precedence,
       adj_assoc: lrn,
       adj_tok: tok,
       adj: result -> result -> result}
      -> resolver

   val no_adj_resolver: 
      {token: tok -> (result, fixity) Sum.sum,
       adj: tok * tok -> exn}
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


   (* EXCEPTIONS *)

   (* Trailing infix or prefix (NONE for empty input): "2+4+", "3+~" *)
   (* Recoverable if more input is available *)
   exception Trailing of tok option * partial_state (* "2+4+", "3+~" *)
 
   (* Successive infix (NONE for leading infix): "+2", "2~+6", "3++6"
    * Also used for the corner case of a too-low-precedence prefix 
    * operator, like "~ sin 5" where sin is lower precedence than ~. *)
   exception Successive of tok option * tok         

   (* Associativities do not match at the same precedence (NONE for prefix): *)
   (* SOME: "a <- b -> c", "2=3=5", same precedence *)
   (* NONE: "~ 2 + 4", same precedence *)
   (* However: "4 + ~ 2", same precedence, is treated as unambiguous *)
   exception MixedAssoc of tok * lrn option * tok * lrn    

   (* End of input reached successfully (allows resumption) *)
   exception Finished of total_state
end
