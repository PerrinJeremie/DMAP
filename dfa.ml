open Regexp

(* définition abstraite d'une construction d'automate déterministe 
   à partir d'une expression régulière *)

module type DFA = sig
  type 'a state
  val init : 'a regexp -> 'a state
  val final : 'a state -> bool
  val next : 'a state -> 'a -> 'a state 
end


(* accepteur associé à une construction d'automate *)
module type ACCEPTOR = sig
  val accept : 'a acceptor
end

module Acceptor = struct

  module Make (DFA:DFA) : ACCEPTOR = struct 

      let accept reg w =
        let rec aux q = function
          | [] -> DFA.final q
          | h :: t -> aux (DFA.next q h) t
        in
        aux (DFA.init reg) w
  end  

end
