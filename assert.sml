(* Runtime invariant checking *)

structure Assert = 
struct
   fun assert i f = 
      if f () then () 
      else raise Fail ("failed assertion ("^Int.toString i^")")
(* fun assert f = () *)
end
