(* Runtime invariant checking *)

structure Assert = 
struct
   fun assert f = if f () then () else raise Fail "failed assertion"
(* fun assert f = () *)
end
