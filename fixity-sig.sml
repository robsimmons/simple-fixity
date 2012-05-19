(* Signature of a fixity resolver that understands infix and prefix. *)

signature FIXITY = 
sig
   type precedence
   type tok
   type result

   datatype lrn = NON | LEFT | RIGHT
   datatype fixity = 
      Prefix of precedence * (result -> result)
    | Infix of precedence * (result -> result -> result)
    | Infixl of precedence * (result -> result -> result)
    | Infixr of precedence * (result -> result -> result)

   (* Internal state of the parser (allows for resumption) *)
   type partial_state (* Needs more input to resolve fixity *)
   type total_state (* Has enough input to resolve fixity *)

   exception Trailing of tok option* partial_state (* "2 + 4 +", "3 + ~" *)
   exception Successive of tok option * tok        (* "", "2~+6", "3++6" *)
   exception ConsecutiveNonInfix of tok * tok      (* "2 = 3 = 5" *)
   exception MixedAssoc of tok * lrn * tok * lrn   (* "a <- b -> c" *)
   exception PrefixEqualInfix of tok * tok         (* "~ 2 + 4", same prec. *) 
   exception SomethingLowPrefix of tok * tok       (* "4 + sin 5 + 9" *)
   exception Finished of total_state               (* "2 + 9 * 12" *)

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

   (* Completely resolve fixity *)
   val resolveStream: resolver -> tok Stream.stream -> result
   val resolveList: resolver -> tok list -> result

   (* Resolve fixity as much as possible, but anticipate more input *)
   val resolve: resolver -> tok Stream.stream -> 'a
   val resumeTotal: resolver -> total_state -> tok Stream.stream -> 'a
   val resumePartial: resolver -> partial_state -> tok Stream.stream -> 'a
   val finalize: resolver -> total_state -> result
end
