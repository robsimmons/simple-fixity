
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

   fun print_stack stack xs = 
      case stack of 
         Bot => String.concatWith " $ " ("Bot" :: xs)
       | S $ DAT _ => print_stack S ("DAT" :: xs)
       | S $ PREFIX _ => print_stack S ("PREFIX" :: xs)
       | S $ INFIX _ => print_stack S ("INFIX" :: xs)

   
   (* Exception interface *)

   type total_state = item stack
   type partial_state = item stack * opt_prec * tok option

   exception Complete of total_state
   exception Incomplete of tok option * partial_state 
   exception Wrong of tok option * tok
   exception Ambiguous of tok * lrn option * tok * lrn option


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
             raise Ambiguous (tok1, SOME NON, tok2, SOME NON)
   in 
    ( Assert.assert 1 (fn () => valid_stack S)
    ; case S of 
         Bot $ DAT d => 
           ((* Finished: dispatch everything *)
            Bot $ DAT (dispatch lrn d xs))
       | S $ PREFIX (prec, tok, f) $ DAT d => 
           ((* Prefix: better be lower prec *)
            if lt (PREC prec, running_prec) 
            then S $ PREFIX (prec, tok, f) $ DAT (dispatch lrn d xs)
            else raise Ambiguous (tok, NONE, last_tok, SOME lrn))
       | S $ INFIX (prec, lrn', tok, f) $ DAT d2 =>
           ((* Infix: better be lower prec or the same associtivity *)
            if lt (PREC prec, running_prec) 
            then S $ INFIX (prec, lrn', tok, f) $ DAT (dispatch lrn d2 xs)
            else if ( Assert.assert 2 (fn () => eq (PREC prec, running_prec))
                    ; lrn = lrn')
            then reduce_infix_at_precedence S (running_prec, lrn, tok) 
                    ((tok, f, d2) :: xs)
            else raise Ambiguous (tok, SOME lrn', last_tok, SOME lrn))
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
    ( Assert.assert 3 (fn () => valid_stack S)
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

   fun shift S str = 
    ( Assert.assert 4 (fn () => valid_stack S)
    ; case Stream.front str of 
         Stream.Nil => raise Complete S
       | Stream.Cons (x as DAT d, str) =>
            raise Fail "Input stream badly formed: adjacent DATs"
       | Stream.Cons (x as INFIX (prec, _, tok, _), str) =>
            must_shift
               (reduce_precedence S (PREC prec) $ x, PREC prec, SOME tok) str
       | Stream.Cons (x as PREFIX (prec, tok, _), xs) => 
            must_shift 
               (reduce_precedence S (PREC prec) $ x, PREC prec, SOME tok) str)

   (* must_shift is called when the stack consists of a valid stack, followed
    * by either an infix or a prefix operator, followed by a series of one or 
    * more prefix operators. *)

   and must_shift (state as (S, top_prec, top_tok)) str = 
    ((* print ("must_shift: "^print_stack S []^"\n"); *)
      Assert.assert 5 (fn () => valid_partial_stack S (top_prec))
    ; Assert.assert 6
        (fn () => isSome top_tok orelse eq (MIN, top_prec))
    ; case Stream.front str of
         Stream.Nil => raise Incomplete (top_tok, state)
       | Stream.Cons (x as DAT d, xs) => shift (S $ x) xs
       | Stream.Cons (x as INFIX (_, _, tok, _), xs) => 
            raise Wrong (top_tok, tok)
       | Stream.Cons (x as PREFIX (prec, tok, f), xs) => 
            if leq (top_prec, PREC prec)
            then must_shift
                    (S $ x, PREC prec, SOME tok)
                    xs
            else raise Ambiguous (valOf top_tok, NONE, tok, NONE))


   (* The resolver type and its introduction forms *)
 
   type resolver = 
      {handle_token: tok -> (result, fixity) Sum.sum,
       adj_prec: unit -> Precedence.t,
       adj_assoc: lrn,
       adj_tok: unit -> tok,
       adj: tok * tok -> result -> result -> result}

   fun AdjResolver {token, adj_prec, adj_assoc, adj_tok, adj}: resolver = 
      {handle_token = token,
       adj_prec = fn () => adj_prec,
       adj_assoc = adj_assoc,
       adj_tok = fn () => adj_tok,
       adj = fn _ => adj}

   fun NoAdjResolver {token, adj}: resolver =
      {handle_token = token,
       adj_prec = fn () => raise Fail "Invariant: adjacency discovered late",
       adj_assoc = LEFT,
       adj_tok = fn () => raise Fail "Invariant: adjacency discovered late",
       adj = fn (t1, t2) => raise (adj (t1, t2))}


   (* Turn a stream of tokens into a stream of results, adding an applicaiton
    * between every two successive tokens. last_tok is NONE if the 
    * last token either didn't exist or was an infix/prefix token. *)
   fun map_stream (resolver: resolver) (last_tok: tok option) str =
   let 
      fun app (last_tok, tok) = 
      let val f = #adj resolver (last_tok, tok)
      in INFIX (#adj_prec resolver (),
                #adj_assoc resolver,
                #adj_tok resolver (), f)
      end
   in Stream.lazy
      (fn () =>
         (case Stream.front str of 
             Stream.Nil => Stream.Nil
           | Stream.Cons (tok, str) => 
               (case (last_tok, #handle_token resolver tok) of
                   (NONE, Sum.INL res) => 
                      Stream.Cons (DAT res,
                         map_stream resolver (SOME tok) str)
                 | (SOME last_tok, Sum.INL res) => 
                      Stream.Cons (app (last_tok, tok),
                         Stream.eager (Stream.Cons (DAT res,
                            map_stream resolver (SOME tok) str)))
                 | (NONE, Sum.INR (Prefix (prec, f))) =>
                      Stream.Cons (PREFIX (prec, tok, f), 
                         map_stream resolver NONE str)
                 | (SOME last_tok, Sum.INR (Prefix (prec, f))) =>
                      Stream.Cons (app (last_tok, tok),
                         Stream.eager (Stream.Cons (PREFIX (prec, tok, f), 
                            map_stream resolver NONE str)))
                 | (_, Sum.INR (Infix (prec, f))) =>
                      Stream.Cons (INFIX (prec, NON, tok, f), 
                         map_stream resolver NONE str)
                 | (_, Sum.INR (Infixr (prec, f))) =>
                      Stream.Cons (INFIX (prec, RIGHT, tok, f), 
                         map_stream resolver NONE str)
                 | (_, Sum.INR (Infixl (prec, f))) =>
                      Stream.Cons (INFIX (prec, LEFT, tok, f), 
                         map_stream resolver NONE str))))
   end


   fun resumePartial resolver (S, prec, tok) str = 
    ( Assert.assert 7 (fn () => valid_partial_stack S prec)
    ; must_shift (S, prec, tok) (map_stream resolver tok str))

   fun resumeTotal resolver S str = 
    ( Assert.assert 8 (fn () => valid_stack S)
    ; shift S (map_stream resolver NONE str)) (* XXX BUG *)

   fun finalize resolver S = 
    ( Assert.assert 9 (fn () => valid_stack S)
    ; case reduce_precedence S MIN of
         Bot $ DAT d => d
       | _ => raise Fail ("Did not reduce fully ["^print_stack S []^"]"))
 
   fun resolve resolver str = 
      must_shift (Bot, MIN, NONE) (map_stream resolver NONE str)

   fun resolveStream resolver str = 
      resolve resolver str
    handle Complete state => finalize resolver state

   fun resolveList resolver toks = 
      resolveStream resolver (Stream.fromList toks)
end

functor IntFixityFn (type tok type result):> 
FIXITY where type precedence = int
         and type tok = tok
         and type result = result =
struct
structure X = SimplestFixityFn 
  (structure Precedence = IntOrdered 
   type tok = tok 
   type result = result)
open X 
end
