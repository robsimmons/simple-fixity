CM.make "$SMACKAGE/cmlib/v1/cmlib.cm";
CM.make "sources.cm";
use "../cmlib/tests/testing.sig";
use "../cmlib/tests/testing.sml";

structure Test = 
struct

   open Fixity

   structure FixityString = 
   IntFixityFn (type tok = string type result = string)

   structure FixityInt =
   IntFixityFn (type tok = string type result = int)

   val () = Testing.reset ()

   val () = 
      Testing.expect true (fn x => x) "automatic"

   fun expect_error f hnd s msg 
      Testing.expect ()
        (fn () =>
         let in ((); false)
          handle exn => hnd exn
         end)
        (if msg = "" then ("'"^s^"' (error expected)")
         else ("'"^s^"' "^msg^" (error expected)"))

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
