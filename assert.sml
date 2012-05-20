(* Runtime invariant checking *)

structure Assert = 
struct
   fun assert i f = 
      if f () then () 
      else raise Fail ("failed assertion ("^Int.toString i^")")
  fun assert i f = () (* Comment to actually check assertions *)
end
