
functor SimplestFixityFn 
  (structure Precedence: ORDERED
   type tok 
   type result ):>
FIXITY where type precedence = Precedence.t 
         and type tok = tok
         and type result = result = 
struct
   type tok = tok
   type result = result
   type precedence = Precedence.t
   datatype fixity = 
      Prefix of Precedence.t * (result -> result)
    | Infix of Precedence.t * (result -> result -> result)
    | Infixl of Precedence.t * (result -> result -> result)
    | Infixr of Precedence.t * (result -> result -> result)

   (* Precedence with an extra top and bottom, and comparison *)

   datatype opt_prec = MIN | PREC of Precedence.t | MAX 

   fun compare x =
      case x of 
         (MIN, MIN) => EQUAL
       | (MIN, _) => LESS
       | (PREC _, MIN) => GREATER
       | (PREC prec1, PREC prec2) => Precedence.compare (prec1, prec2)
       | (MAX, MAX) => EQUAL
       | (_, MAX) => LESS  
       | (MAX, _) => GREATER

   fun eq x = EQUAL = compare x
   fun lt x = LESS = compare x
   fun leq x = GREATER <> compare x


   (* Generic stacks (snoc lists) and the stack items we'll put on them *)

   datatype 'a stack
     = Bot
     | $ of 'a stack * 'a

   infix 2 $
 
   datatype lrn = LEFT | RIGHT | NON

   datatype item = 
      DAT of result
    | PREFIX of Precedence.t * tok * (result -> result)
    | INFIX of Precedence.t * lrn * tok * (result -> result -> result)

   
   (* Exception interface *)

   type total_state = item stack
   type partial_state = item stack * Precedence.t * tok

   exception EmptyParse
   exception Trailing of tok * partial_state
   exception Successive of tok * tok
   exception ConsecutiveNonInfix of tok * tok
   exception MixedAssoc of tok * lrn * tok * lrn
   exception PrefixEqualInfix of tok * tok
   exception SomethingLowPrefix of tok * tok
   exception Finished of total_state

   (* valid_stack checks for the well-formedness of a shift-reduce
    * parse stack.
    * 
    * A shift-reduce stack is well-formed if it represents a complete
    * parse in which the operators are (non-strictly) ordered from
    * lowest to highest. This invariant makes it impossible to parse
    * some technically unambiguous strings, like "- sin - 5", where
    * "-" is unary minus and "sin" is the unary sin function and the
    * two have different precedence. If unary minus has lower
    * precedence (binds less tightly) than sin, we must write "- sin
    * (- 5)". Alternatively, if unary minus has higher precedence
    * (binds more tightly) than sin, we must write "- (sin - 5)". (If
    * the two have the same precedence, then "- sin - 5" can parse
    * correctly.)
    *
    * Certainly, it would be possible to allow "- sin - 5" to parse
    * meaningfully, even if the two had different precedences, but the
    * resulting invariant would be more complicated, and just because
    * something is possible that doesn't mean it's a good idea. *)

   fun valid_data x = 
      case x of 
         DAT _ => true
       | _ => false

   fun valid_partial_stack S high = 
      case S of 
         Bot (* $ d1 *) => true
       | S $ PREFIX (prec, _, _) (* $ d1 *) =>
            leq (PREC prec, high) 
            andalso valid_partial_stack S (PREC prec) 
       | S $ d1 $ INFIX (prec, _, _, _) (* $ d2 *) =>
            valid_data d1 
            andalso leq (PREC prec, high) 
            andalso valid_partial_stack S (PREC prec)
       | _ => false

   fun valid_stack S =
      case S of 
         Bot => false (* Empty stack is ill-formed *)
       | S $ d => valid_data d andalso valid_partial_stack S MAX

   (* Reduce a series of left-associative operations *)
   fun reduce_left (x: 'a) (ys: (tok * ('a -> 'a -> 'a) * 'a) list): 'a = 
      case ys of 
         [] => x
       | ((_, f, y) :: ys) => reduce_left (f x y) ys


   (* Reduce a series of right-associatve operations *)
   fun reduce_right (x: 'a) (ys: (tok * ('a -> 'a -> 'a) * 'a) list): 'a =
      case ys of 
         [] => x
       | ((_, f, y) :: ys) => f x (reduce_right y ys)


   (* reduce_infix_at_precedence
    * 
    * If we want to reduce an infix operator, we collect all the
    * operators at the top of the stack that have the same precedence
    * on the stack and reduce them at the same time. The resulting
    * stack will have a strictly lower maximum precedence.
    * 
    * Rough example (/ and * are both left associative with same fixity, 6):
    *
    * If we call
    * reduce_infix_at_precedence 
    *    (Bot $ 4 $ + $ 6 $ * $ 12 $ / $ 16) 
    *    LEFT * 6 [ ("/", ..., 9), ("*", ..., 2) ]
    *     
    * the result will be 
    * Bot $ 4 $ + $ ((((6*12)/16)/9)*2) 
    * *)

   fun reduce_infix_at_precedence S (running_prec, lrn, last_tok) xs = 
   let
      fun dispatch LEFT x xs = reduce_left x xs
        | dispatch RIGHT x xs = reduce_right x xs
        | dispatch NON x [ (_, f, y) ] = f x y
        | dispatch NON x [] = raise Fail "Invariant: xs must be nonempty"
        | dispatch NON x ((tok1, _, _) :: (tok2, _, _) :: _) = 
             raise ConsecutiveNonInfix (tok1, tok2)
   in 
    ( Assert.assert (fn () => valid_stack S)
    ; case S of 
         Bot $ DAT d => 
           ((* Finished: dispatch everything *)
            Bot $ DAT (dispatch lrn d xs))
       | S $ PREFIX (prec, tok, f) $ DAT d => 
           ((* Prefix: better be lower prec *)
            if lt (PREC prec, running_prec) 
            then S $ PREFIX (prec, tok, f) $ DAT (dispatch lrn d xs)
            else raise PrefixEqualInfix (tok, last_tok))
       | S $ INFIX (prec, lrn', tok, f) $ DAT d2 =>
           ((* Infix: better be lower prec or the same associtivity *)
            if lt (PREC prec, running_prec) 
            then S $ INFIX (prec, lrn', tok, f) $ DAT (dispatch lrn d2 xs)
            else if ( Assert.assert (fn () => eq (PREC prec, running_prec))
                    ; lrn = lrn')
            then reduce_infix_at_precedence S (running_prec, lrn, tok) 
                    ((tok, f, d2) :: xs)
            else raise MixedAssoc (tok, lrn', last_tok, lrn))
       | _ => raise Fail "Impossible? (Should be precluded by assertion)")
   end


   (* If we want to add an operator to a valid stack, we have to make sure
    * everything further down in the stack has lower or equal precedence, 
    * reducing the overall precedence of the stack.
    * 
    * requires: valid_stack S
    * returns: (S', top_tok, top_prec) where S' is another valid stack whose 
    *   maximum precedence is top_prec <= required based on the token
    *   top_tok. *)         

   fun reduce_precedence S required = 
    ( Assert.assert (fn () => valid_stack S)
    ; case S of 
         Bot $ d => S
       | S' $ PREFIX (prec, tok, f) $ DAT d => 
           (if leq (PREC prec, required) 
            then S
            else reduce_precedence (S' $ DAT (f d)) required)
       | S' $ INFIX (prec, lrn, tok, f) $ DAT d2 => 
           (if leq (PREC prec, required) 
            then S
            else reduce_precedence
                    (reduce_infix_at_precedence S' (PREC prec, lrn, tok) 
                        [(tok, f, d2)])
                    required)
       | _ => raise Fail "Impossible? (Should be caught by assertion.)")


   (* shift is called on an arbitrary valid stack and list of tokens *)

   fun shift (app as (app_prec, _, _, _)) S str = 
    ( Assert.assert (fn () => valid_stack S)
    ; case Stream.front str of 
         Stream.Nil => raise Finished S
       | Stream.Cons (x as DAT d, str) =>
            shift app
               (reduce_precedence S (PREC app_prec) $ INFIX app $ x) str
       | Stream.Cons (x as INFIX (prec, _, tok, _), str) =>
            must_shift app
               (reduce_precedence S (PREC prec) $ x, prec, tok) str
       | Stream.Cons (x as PREFIX (prec, tok, _), xs) => 
            must_shift app
               (reduce_precedence S (PREC prec) $ x, prec, tok) str)

   (* must_shift is called when the stack consists of a valid stack, followed
    * by either an infix or a prefix operator, followed by a series of one or 
    * more prefix operators. *)

   and must_shift app (state as (S, top_prec, top_tok)) str = 
    ( Assert.assert (fn () => valid_partial_stack S (PREC top_prec))
    ; case Stream.front str of
         Stream.Nil => raise Trailing (top_tok, state)
       | Stream.Cons (x as DAT d, xs) => shift app (S $ x) xs
       | Stream.Cons (x as INFIX (_, _, tok, _), xs) => 
            raise Successive (top_tok, tok)
       | Stream.Cons (x as PREFIX (prec, tok, f), xs) => 
            if leq (PREC top_prec, PREC prec)
            then must_shift app
                    ((reduce_precedence S (PREC prec)) $ x, prec, tok)
                    xs
            else raise SomethingLowPrefix (top_tok, tok))

   type resolver = 
      {handle_token: tok -> (result, fixity) Sum.sum,
       adj_prec: precedence,
       adj_assoc: lrn,
       adj_tok: tok,
       adj: result -> result -> result}

   fun get_app (x: resolver) =
      (#adj_prec x, #adj_assoc x, #adj_tok x, #adj x)
end

functor IntFixityFn (type tok type result) = 
struct
structure X = SimplestFixityFn 
  (structure Precedence = IntOrdered 
   type tok = tok 
   type result = result)
open X 
end
